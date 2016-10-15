package com.haskforce.tools.cabal.actions

import java.io.File

import com.haskforce.Implicits._
import com.haskforce.system.packages.{AlreadyRegistered, FileError, ShadowedByRegistered}
import com.haskforce.system.utils.{FileUtil, NotificationUtil}
import com.haskforce.tools.cabal.CabalExecutor
import com.haskforce.tools.cabal.packages.CabalPackageManager
import com.haskforce.tools.cabal.settings.AddCabalPackageOptions
import com.haskforce.tools.cabal.settings.ui.{AddCabalPackageDialog, AddCabalPackageUtil}
import com.intellij.notification.NotificationType
import com.intellij.notification.NotificationType._
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.fileEditor.OpenFileDescriptor
import com.intellij.openapi.project.{DumbAware, Project}
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFileManager}

import scala.util.{Failure, Success, Try}

class AddCabalPackageAction extends AnAction with DumbAware {
  def actionPerformed(e: AnActionEvent) {
    Option(e.getProject).fold {
      NotificationUtil.displaySimpleNotification(
        WARNING, null, AddCabalPackageAction.TITLE,
        "Could not add cabal file since you are not currently in a project."
      )
    } { project =>
      val dialog = new AddCabalPackageDialog(project, AddCabalPackageAction.createCallback(project))
      dialog.setVisible(true)
    }
  }
}

object AddCabalPackageAction {
  val TITLE: String = "Add Cabal Package"

  private def createCallback(project: Project)(getOptions: Project => AddCabalPackageOptions): Unit = {
    val options = getOptions(project)
    def display(notificationType: NotificationType, message: String) = {
      NotificationUtil.displaySimpleNotification(notificationType, project, TITLE, message)
    }
    val file = new File(options.rootDir)
    if (!file.isDirectory) {
      display(ERROR, s"Cabal directory: ${options.rootDir} is not an directory")
    }
    ApplicationManager.getApplication.runWriteAction({ () =>
      val cabal = CabalExecutor.create(project, Some(options.rootDir)).right.getOrElse {
        display(ERROR, s"")
        return
      }
      if (!file.exists() && !file.mkdirs()) {
        display(ERROR, s"Unable to create directory for file ${options.rootDir} ")
        return
      }
      Try {
        cabal.init(project, AddCabalPackageUtil.buildArgs(options))
      } match {
        case Success(message) => display(INFORMATION, message)
        case Failure(e) => {
          display(ERROR, e.getMessage)
          return
        }
      }
    }: Runnable)
    VirtualFileManager.getInstance.asyncRefresh { () =>
      val newCabalFilePath: String = FileUtil.join(options.rootDir, options.packageName + ".cabal")
      Option(LocalFileSystem.getInstance.refreshAndFindFileByPath(newCabalFilePath)) match {
        case None => display(WARNING, s"Could not find new cabal file at $newCabalFilePath - may not have been created.")
        case Some(cabalFile) =>
          CabalPackageManager.registerNewPackage(new File(newCabalFilePath), project, replace = true) match {
            case Left(FileError(location, name, message)) =>
              display(ERROR, s"registering $name failed with error message: $message")
            case Left(ShadowedByRegistered(violatingSourceDir, shadowing, shadowingContentRoot, toRegister)) =>
              display(ERROR, s"unable to create Module, package source root: ${violatingSourceDir.getCanonicalPath} is shadowed by content root: ${shadowingContentRoot.getCanonicalPath} of module ${shadowing.getName}")
            //should not happen
            case Left(AlreadyRegistered(existing, toRegister)) => display(ERROR, s"unable to update module ${existing.getName}")
            case Right(_) => new OpenFileDescriptor(project, cabalFile).navigate(true)
          }
      }
    }
  }
}
