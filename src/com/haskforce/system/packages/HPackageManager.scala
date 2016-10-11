package com.haskforce.system.packages

import com.haskforce.system.settings.HaskellBuildSettings
import com.haskforce.system.utils.ExecUtil.ExecError
import com.haskforce.system.utils.{ExecUtil, FileUtil, ModulesUtil}
import com.haskforce.tools.cabal.packages.CabalPackageManager
import com.haskforce.tools.stack.packages.StackPackageManager
import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.module.{Module, ModuleManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.openapi.vfs.{LocalFileSystem, VfsUtilCore, VirtualFile}
/**
  * common functionality to interact with the packages
  */
class HPackageManager(intellijProject: Project) {
  private val LOG = Logger.getInstance(HPackageManager.getClass)
  //no public API-Access! this functionality should be replaced by proper project-management
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

  /**
    * loads a package from a state
    * @param state the state to load from
    * @param location the location of the config-file, relative to the project root
    * @return the recovered HPackage
    */
  def loadPackage(state: HPackageState, location: String): Option[HPackage] = {
    HPackageManager.packageManagers.find(pkgMngr => pkgMngr.getName == state.packageManager)
      .flatMap(pkgMngr => {
        val resolvedLocation: Option[VirtualFile] = FileUtil.fromRelativePath(location, intellijProject)
        if (resolvedLocation.isEmpty) {
          LOG.error(s"unable to resolve location for $location, cannot load module")
        }
        resolvedLocation.flatMap(file => pkgMngr.getPackageFromState(state, file, intellijProject))
      })
  }

  /**
    * returns all existing packages and their modules
    */
  def getExistingPackages: List[(HPackage, Module)] = {
    ModuleManager.getInstance(intellijProject).getModules.toList
      .flatMap(module => HPackageModule.getInstance(module).optPackage.map(pkg => (pkg, module)))
  }

  /**
    * fallback GHC version
    */
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

  /**
    * sets the main Package (used for fallback if the module has no package associated)
    */
  def setMainPackage(hPackage: HPackage): Unit = {
    this.mainPackage = Some(hPackage)
  }
}

object HPackageManager {
  private val packageManagers = List(CabalPackageManager, StackPackageManager)

  /**
    * returns the matching instance
    */
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
case class AlreadyRegistered(module: Module, toRegister: HPackage) extends RegisterError
case class ShadowedByRegistered(violatingSourceDir: VirtualFile, shadowing: Module, shadowingContentRoot: VirtualFile, toRegister: HPackage) extends RegisterError
