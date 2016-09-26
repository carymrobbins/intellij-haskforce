package com.haskforce.tools.cabal.actions

import com.haskforce.haskell.HaskellModuleType
import com.haskforce.Implicits._
import com.haskforce.tools.cabal.settings.ui.{AddCabalPackageUtil, DiscoverCabalPackagesDialog}
import com.haskforce.system.utils.{FileUtil, NotificationUtil}
import com.intellij.notification.NotificationType
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.project.{DumbAware, Project}
import com.intellij.openapi.vfs.VirtualFile

import scala.collection.JavaConversions._

/**
 * Finds Cabal packages within project which are lacking an IntelliJ modules and creates modules for them.
 */
class DiscoverCabalPackagesAction extends AnAction with DumbAware {
  import DiscoverCabalPackagesAction._

  override def actionPerformed(e: AnActionEvent): Unit = {
    Option(e.getProject).fold { onNoProject() } { run }
  }
}

object DiscoverCabalPackagesAction {
  val TITLE = "Discover Cabal Packages"

  def run(project: Project): Unit = {
      findDiscoverableCabalFiles(project) match {
        case Nil => onNoCabalFiles()
        case cabalFiles => displayForm(project, cabalFiles)
      }
  }

  private def onNoProject(): Unit = {
    NotificationUtil.displaySimpleNotification(
      NotificationType.WARNING, null, TITLE,
      "Could not discover Cabal files since you are not currently in a project."
    )
  }

  private def onNoCabalFiles(): Unit = {
    NotificationUtil.displaySimpleNotification(
      NotificationType.WARNING, null, TITLE,
      "Could not find any undiscovered Cabal files."
    )
  }

  private def onSuccess(packageNames: Seq[String]): Unit = {
    packageNames match {
      case Nil =>
        NotificationUtil.displaySimpleNotification(
          NotificationType.WARNING, null, TITLE,
          "No Cabal packages imported."
        )
      case _ =>
        NotificationUtil.displaySimpleNotification(
          NotificationType.INFORMATION, null, TITLE,
          "Successfully imported Cabal packages:<br/>" + packageNames.mkString("<br/>")
        )
    }
  }

  /**
   * Finds Cabal files which do not belong to a module.
   */
  private def findDiscoverableCabalFiles(project: Project): Seq[VirtualFile] = {
    Option(project.getBaseDir).map(
      FileUtil.findFilesRecursively(_, _.getExtension == "cabal").filter(isDiscoverable(project))
    ).getOrElse(Seq())
  }

  /**
   * Determines whether a package can be discovered.  If is has an associated module, it is not discoverable.
   * Packages are associated with modules if the module file is in the same directory as the Cabal file.
   */
  private def isDiscoverable(project: Project)(file: VirtualFile): Boolean = {
    !HaskellModuleType.findModules(project).exists(m =>
      Option(m.getModuleFile).map(_.getParent.getCanonicalPath).contains(file.getParent.getCanonicalPath)
    )
  }

  private def displayForm(project: Project, cabalFiles: Seq[VirtualFile]): Unit = {
    new DiscoverCabalPackagesDialog(project, cabalFiles, importCabalPackages(project))
  }

  private def importCabalPackages(project: Project)(files: Seq[VirtualFile]): Unit = {
    ApplicationManager.getApplication.runWriteAction({ () =>
      files.foreach(AddCabalPackageUtil.importCabalPackage(project))
      onSuccess(files.map(_.getNameWithoutExtension))
    }: Runnable)
  }
}
