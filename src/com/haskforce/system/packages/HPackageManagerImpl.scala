package com.haskforce.system.packages

import com.haskforce.system.settings.HaskellBuildSettings
import com.haskforce.system.utils.ExecUtil.ExecError
import com.haskforce.system.utils.{ExecUtil, NotificationUtil}
import com.haskforce.tools.cabal.packages.CabalPackageManager
import com.haskforce.tools.stack.packages.StackPackageManager
import com.intellij.notification.NotificationType
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.{LocalFileSystem, VfsUtilCore, VirtualFile}

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

  override def replaceMainPackage(packageManager: BackingPackageManager, file: String): Either[FileError, List[FileError]] = {
    val virtualFile: VirtualFile = LocalFileSystem.getInstance().findFileByPath(file)
    if (virtualFile == null) {
      Left(FileError(file, "unknown", s"unable to obtain VirtualFile for file $file"))
    } else {
      packageManager.replaceMain(virtualFile, getMainPackage, intellijProject)
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
        case Some(existing) => {
          if (existing.getPackageManager == hPackage.getPackageManager) {
            existing.emitEvent(Update(hPackage))
          } else {
            existing.emitEvent(Replace(hPackage))
          }
        }
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
    val handleResult = (result: Either[FileError, List[FileError]]) => {
      if (result.isLeft) {
        settings.setUseStack(false)
        val error: FileError = result.left.get
        NotificationUtil.displaySimpleNotification(NotificationType.ERROR,
          intellijProject,
          "Error while initializing Haskforce",
          s"Unable to initialized Haskforce properly, please reset the compiler settings." +
            s"<br/>Error in File ${error.location}, Message: ${error.errorMsg}")
      } else {
        val errors: List[FileError] = result.right.get
        if (errors.nonEmpty) {
          NotificationUtil.displaySimpleNotification(NotificationType.ERROR,
            intellijProject,
            "Error while initializing Haskforce",
            "Unable to register all packages correctly.<br/>Errors:<br/>"
              + errors.map(error => s"in package: ${error.location} error: ${error.errorMsg}")
              .mkString("<br/>")
          )
        }
      }
    }
    if (settings.isStackEnabled) {
      settings.setUseCabal(false)
      val result: Either[FileError, List[FileError]] = replaceMainPackage(StackPackageManager, settings.getStackFile)
      if (result.isLeft) {
        settings.setUseStack(false)
      }
      handleResult(result)
    } else if (settings.isCabalEnabled) {
      val result: Either[FileError, List[FileError]] = replaceMainPackage(CabalPackageManager, settings.getStackFile)
      if (result.isLeft) {
        settings.setUseCabal(false)
      }
      handleResult(result)
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
