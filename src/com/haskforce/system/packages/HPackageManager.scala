package com.haskforce.system.packages

import com.haskforce.system.settings.HaskellBuildSettings
import com.haskforce.system.utils.ExecUtil.ExecError
import com.haskforce.system.utils.{ExecUtil, ModulesUtil}
import com.haskforce.tools.cabal.packages.CabalPackageManager
import com.haskforce.tools.stack.packages.StackPackageManager
import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.module.{Module, ModuleManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.openapi.vfs.{LocalFileSystem, VfsUtilCore, VirtualFile}
/**
  * common functionality to interact with the packages
  */
class HPackageManager(intellijProject: Project) {
  //not for API-Access!
  private[packages] var mainPackage: Option[HPackage] = initMainPackage()

  private def initMainPackage(): Option[HPackage] = {
    val settings: HaskellBuildSettings = HaskellBuildSettings.getInstance(intellijProject)
    if (settings.isStackEnabled) {
      for {
        stackFile <- Option(LocalFileSystem.getInstance().findFileByPath(settings.getStackFile))
        results <- StackPackageManager.getPackages(stackFile, intellijProject).right.toOption
        hPackage <- results.flatMap(_.right.toOption).headOption
      } yield hPackage
    } else if (settings.isCabalEnabled) {
      for {
        stackFile <- Option(LocalFileSystem.getInstance().findFileByPath(settings.getStackFile))
        results <- CabalPackageManager.getPackages(stackFile, intellijProject).right.toOption
        hPackage <- results.flatMap(_.right.toOption).headOption
      } yield hPackage
    } else {
      None
    }
  }

  def getPackage(state: HPackageState, moduleDirectory: VirtualFile, project: Project): Option[HPackage] = {
    HPackageManager.packageManagers.find(pkgMngr => pkgMngr.getName == state.getPackageManager)
      .flatMap(pkgMngr => pkgMngr.getPackageFromState(state, moduleDirectory, project))
  }

  def getExistingPackages: List[(HPackage, Module)] = {
    ModuleManager.getInstance(intellijProject).getModules.toList
      .flatMap(module => HPackageModule.getInstance(module).getPackage.map(pkg => (pkg, module)))
  }

  def getPackageForConfigFile(file: VirtualFile): Either[SearchResultError, (HPackage, Module)] = {
    val findMatching: (Module, Option[HPackage]) => Option[Either[SearchResultError, (HPackage, Module)]] = (module: Module, optPackage: Option[HPackage]) => {
      if (optPackage.isDefined) {
        val hPackage = optPackage.get
        if (hPackage.getLocation == file) {
          Some(Right((hPackage, module)))
        } else if (hPackage.getLocation.getParent == file.getParent) {
          Some(Left(AlreadyRegisteredResult(hPackage, module)))
        } else {
          val roots: Set[VirtualFile] = ModuleRootManager.getInstance(module).getContentRoots.toSet
          if (roots.exists(root => VfsUtilCore.isAncestor(root, file, false))) {
            Some(Left(Shadowed(module))): Option[Either[SearchResultError, HPackage]]
          }
          roots
            .map(root => root.getParent)
            .find(root => root == file.getParent)
            .map(_ => Left(NotYetRegistered(module)))
        }
      } else {
        val roots: Set[VirtualFile] = ModuleRootManager.getInstance(module).getContentRoots.toSet
        if (roots.exists(root => VfsUtilCore.isAncestor(root, file, false))) {
          Some(Left(Shadowed(module)))
        } else {
          None
        }
      }
    }
    val headOption: Option[Either[SearchResultError, (HPackage, Module)]] = ModuleManager.getInstance(intellijProject).getModules.toList
      .map(module => (module, HPackageModule.getInstance(module).getPackage))
      .flatMap(tuple => findMatching(tuple._1, tuple._2))
      .headOption
    headOption match {
      case Some(x) => x
      case None => Left(NoModuleYet())
    }
  }

  def retrieveDefaultGHCVersion(): Either[ExecUtil.ExecError, GHCVersion] = {
    val settings: HaskellBuildSettings = HaskellBuildSettings.getInstance(intellijProject)
    val path: Either[ExecUtil.ExecError, String] = settings.getGhcPath match {
      case null => Left(new ExecError("No GHC-path configured", null))
      case "" => Left(new ExecError("GHC path is empty", null))
      case x => Right(x)
    }

    path
      .right.flatMap(path => GHCVersion.getGHCVersion(null, path))
  }

  def setMainPackage(hPackage: HPackage): Unit = {
    this.mainPackage = Some(hPackage)
  }
}
object HPackageManager {
  private val packageManagers = List(CabalPackageManager, StackPackageManager)

  def getInstance(project: Project) = {
    ServiceManager.getService(project, classOf[HPackageManager])
  }
}

sealed trait SearchResultError
case class Shadowed(module: Module) extends SearchResultError
case class NotYetRegistered(module: Module) extends SearchResultError
case class AlreadyRegisteredResult(other: HPackage, module: Module) extends SearchResultError
case class NoModuleYet() extends SearchResultError

sealed trait RegisterError
case class FileError(location: String, fileName : String, errorMsg: String) extends RegisterError
case class AlreadyRegistered(hPackage: HPackage) extends RegisterError
