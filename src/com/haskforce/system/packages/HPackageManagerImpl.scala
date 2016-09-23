package com.haskforce.system.packages

import java.io.File

import com.haskforce.system.packages.PackageManager.{Cabal, Stack}
import com.haskforce.system.settings.HaskellBuildSettings
import com.haskforce.system.utils.{ExecUtil, NotificationUtil}
import com.haskforce.system.utils.ExecUtil.ExecError
import com.haskforce.tools.cabal.packages.CabalPackageManager
import com.intellij.notification.NotificationType
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.{VfsUtilCore, VirtualFile}

/**
  * this class retrieve, add and remove the active package
  */
class HPackageManagerImpl(intellijProject: Project) extends HPackageManager {
  private var packages : Map[VirtualFile, HPackage] = Map()
  private var mainPackage : HPackage = _

  override def getPackages : Iterable[HPackage] = packages.values

  override def getPackage(file: VirtualFile): Option[HPackage] = packages.get(file)

  override def getMainPackage : Option[HPackage] = Option(mainPackage)

  override def setMainPackage(hPackage: HPackage) = {
    mainPackage = hPackage
  }

  override def replaceMainPackage(packageManager: PackageManager, file: String): Either[FileError, (Option[HPackage], HPackage)] = {
    val set = (hPackage: HPackage) => {
      val replaced: Option[HPackage] = replacePackage(hPackage)
      setMainPackage(hPackage)
      Right((replaced, hPackage))
    }
    val handleFunc = (registerResult: Either[RegisterError, HPackage]) => {
      if (registerResult.isRight) {
        set(registerResult.right.get)
      } else {
        registerResult.left.get match {
          case AlreadyRegistered(hPackage) => set(hPackage)
          case FileError(location, fileName, errorMsg) => Left(FileError(location, fileName, errorMsg))
        }
      }
    }
    packageManager match {
      case Cabal => {
        //TODO is this right???
        val registerResult: Either[RegisterError, HPackage] = CabalPackageManager.registerNewPackage(new File(file), intellijProject)
        handleFunc(registerResult)
      }
      case Stack => {
        ??? //TODO: impl for stack
      }
    }
  }

  override def getDefaultGHCVersion(project: Project): Either[ExecUtil.ExecError, GHCVersion] = {
    val settings: HaskellBuildSettings = HaskellBuildSettings.getInstance(project)
    val path: Either[ExecUtil.ExecError, String] = settings.getGhcPath match {
      case null => Left(new ExecError("No GHC-path configured", null))
      case "" => Left(new ExecError("GHC path is empty", null))
      case x => Right(x)
    }

    path
      .right.flatMap(path => GHCVersion.getGHCVersion(null, path))
  }

  override def addPackage(hPackage : HPackage) : Boolean = {
    if (hPackage.getLocation == null || hPackage.getLocation.isDirectory) {
      return false
    }
    this.synchronized {
      if (packages.contains(hPackage.getLocation)) {
        return false
      } else {
        packages = packages + ((hPackage.getLocation, hPackage))
        return true
      }
    }
  }

  override def replacePackage(hPackage: HPackage): Option[HPackage] = {
    if (hPackage.getLocation == null || hPackage.getLocation.isDirectory) {
      return None
    }
    this.synchronized {
      val toReplace = packages.get(hPackage.getLocation)
      packages = packages + ((hPackage.getLocation, hPackage))
      toReplace match {
        case Some(existing) => existing.emitEvent(Replace(hPackage))
      }
      toReplace
    }
  }

  override def removePackage(hPackage : HPackage) : Boolean = {
    this.synchronized {
      if (!packages.contains(hPackage.getLocation)) {
        return false
      } else {
        packages = packages - hPackage.getLocation
        hPackage.emitEvent(Remove())
        return true
      }
    }
  }

  override def getPackageForFile(file: VirtualFile): Option[HPackage] = {
    packages.toList.map(tuple => (tuple._1.getParent, tuple._2))
      .filter(tuple => VfsUtilCore.isAncestor(tuple._1, file, false))
      .reduceOption((tuple1, tuple2) => {
        if (VfsUtilCore.isAncestor(tuple1._1, tuple2._1, false)) {
          tuple2
        } else {
          tuple1
        }
      })
      .map(tuple => tuple._2)
  }

  override def projectOpened(): Unit = {}

  override def projectClosed(): Unit = {}

  override def initComponent(): Unit = {
    val settings: HaskellBuildSettings = HaskellBuildSettings.getInstance(intellijProject)
    val printError = (msg : String) => {
      NotificationUtil.displaySimpleNotification(NotificationType.ERROR,
        intellijProject,
        "Unable to initialized Haskforce properly, please reset the compiler settings",
        msg)
    }
    if (settings.isCabalEnabled) {
      val result = replaceMainPackage(Cabal, settings.getCabalPath)
      if (result.isLeft) {
        printError(result.left.get.errorMsg)
        settings.setUseCabal(false)
      }
    } else if (settings.isStackEnabled) {
      //TODO impl stack
    }
  }

  override def disposeComponent(): Unit = {}

  override def getComponentName: String = HPackageManagerImpl.NAME
}

object HPackageManagerImpl {
  val NAME = "haskforce.hPackageManager"
}

sealed trait RegisterError
case class FileError(location: String, fileName : String, errorMsg: String) extends RegisterError
case class AlreadyRegistered(hPackage: HPackage) extends RegisterError
