package com.haskforce.tools.cabal.actions

import com.haskforce.Implicits._
import com.haskforce.system.packages._
import com.haskforce.system.utils.{FileUtil, NotificationUtil}
import com.haskforce.tools.cabal.packages.CabalPackageManager
import com.haskforce.tools.cabal.settings.ui.DiscoverCabalPackagesDialog
import com.intellij.notification.NotificationType
import com.intellij.openapi.actionSystem.{AnAction, AnActionEvent}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.{DumbAware, Project}
import com.intellij.openapi.util.Computable
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

  private def importPackages(cabalFiles: Seq[VirtualFile], project: Project): Unit = {
    cabalFiles match {
      case Nil =>
        NotificationUtil.displaySimpleNotification(
          NotificationType.WARNING, null, TITLE,
          "No Cabal packages to import."
        )
      case _ =>
        val (fileErrors, regErrors, shadowed, successes) = cabalFiles
          .map(file => CabalPackageManager.registerNewPackage(file, project, update = true))
          .foldRight((List[FileError](), List[AlreadyRegistered](), List[ShadowedByRegistered](), List[HPackage]()))((either, akk) => {
            val (fileAkk, regAkk, shadowAkk, packAkk) = akk
            either match {
              case Left(f: FileError) => (f :: fileAkk, regAkk, shadowAkk, packAkk)
              case Left(a: AlreadyRegistered) => (fileAkk, a :: regAkk, shadowAkk, packAkk)
              case Left(s: ShadowedByRegistered) => (fileAkk, regAkk, s :: shadowAkk, packAkk)
              case Right(x) => (fileAkk, regAkk, shadowAkk, x :: packAkk)
            }
          })

        fileErrors match {
          case Nil =>
          case _ =>
            val errorToString = (fileError: FileError) => s"${fileError.location} failed: ${fileError.errorMsg}"

            NotificationUtil.displaySimpleNotification(
              NotificationType.ERROR, null, TITLE,
              "Unable to register the following Cabal packages:<br/>" + fileErrors.map(errorToString).mkString("<br/>")
            )
        }

        val regErrorsDisplay: String = regErrors match {
          case Nil => ""
          case _ => "<br/>Unable to update the modules: <br/>" + regErrors
            .map(regError => s"module ${regError.module.getName} to package ${regError.toRegister.getName.getOrElse(regError.toRegister.getLocation.getNameWithoutExtension)}")
            .mkString("<br/>")
        }

        val shadowedDisplay: String = shadowed match {
          case Nil => ""
          case _ =>
            val errorToString = (shadowedError: ShadowedByRegistered) => {
              val toRegister: HPackage = shadowedError.toRegister
              s"the source-directory ${shadowedError.violatingSourceDir} of package " +
                s"${toRegister.getName.getOrElse(toRegister.getLocation.getNameWithoutExtension)} is shadowed by the" +
                s"content-root ${shadowedError.shadowingContentRoot} of module ${shadowedError.shadowing.getName}"
            }
            "<br/>Unable to create the some modules, since they are shadowed by existing modules: <br/>" + shadowed
            .map(errorToString)
            .mkString("<br/>")
        }


        val successesDisplay: String = successes
          .map(project => project.getName.getOrElse(project.getLocation.getNameWithoutExtension))
          .mkString("<br/>")

        NotificationUtil.displaySimpleNotification(
          NotificationType.INFORMATION, null, TITLE,
          "Successfully imported Cabal packages:<br/>" + successesDisplay + regErrorsDisplay + shadowedDisplay
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
    * its not shadowed or already existing
    */
  private def isDiscoverable(project: Project)(configFile: VirtualFile): Boolean = {
    ApplicationManager.getApplication.runReadAction(new Computable[Boolean] {
      override def compute(): Boolean = {
        val result: Either[SearchResultError, (HPackage, Module)] = ProjectSetup.getPackageForConfigFile(configFile, project)
        if (result.isRight) {
          val (existingPackage, existingModule) = result.right.get
          existingPackage.getLocation != configFile || existingPackage.getPackageManager != CabalPackageManager
        } else {
          result.left.get match {
            //unable to handle
            case Shadowed(_) => false
            //can handle
            case other  => true
          }
        }
      }
    })
  }

  private def displayForm(project: Project, cabalFiles: Seq[VirtualFile]): Unit = {
    new DiscoverCabalPackagesDialog(project, cabalFiles, importCabalPackages(project))
  }

  private def importCabalPackages(project: Project)(files: Seq[VirtualFile]): Unit = {
    ApplicationManager.getApplication.runWriteAction({ () =>
      importPackages(files, project)
    } : Runnable)
  }
}
