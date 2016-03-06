package com.haskforce.actions.cabal

import com.haskforce.Implicits._
import com.haskforce.cabal.settings.AddCabalPackageOptions
import com.haskforce.cabal.settings.ui.{AddCabalPackageUtil, AddCabalPackageDialog}
import com.haskforce.utils.{CabalExecutor, FileUtil, NotificationUtil}
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
    ApplicationManager.getApplication.runWriteAction({ () =>
      val cabal = CabalExecutor.create(project, Some(options.rootDir)).right.getOrElse {
        display(ERROR, "Could not generate cabal file, path to cabal not configured in compiler settings.")
        return
      }
      Try {
        maybeCreateModule(project, options)
        cabal.init(project, AddCabalPackageUtil.buildArgs(options))
      } match {
        case Success(message) => display(INFORMATION, message)
        case Failure(e) => display(ERROR, e.getMessage)
      }
    }: Runnable)
    VirtualFileManager.getInstance.asyncRefresh { () =>
      val newCabalFilePath: String = FileUtil.join(options.rootDir, options.packageName + ".cabal")
      Option(LocalFileSystem.getInstance.refreshAndFindFileByPath(newCabalFilePath)) match {
        case None => display(WARNING, s"Could not find new cabal file at $newCabalFilePath - may not have been created.")
        case Some(cabalFile) => new OpenFileDescriptor(project, cabalFile).navigate(true)
      }
    }
  }

  /**
   * If we are creating a cabal package where a module does not currently exist, create it as a Haskell module.
   */
  private def maybeCreateModule(project: Project, options: AddCabalPackageOptions) {
    if (options.maybeModule.isDefined) return
    AddCabalPackageUtil.importCabalPackage(project, options.rootDir, options.packageName)
  }
}
