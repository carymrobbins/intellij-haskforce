package com.haskforce.system.packages

import com.haskforce.system.utils.{FileUtil, ModulesUtil}
import com.intellij.openapi.components._
import com.intellij.openapi.module.{Module, ModuleManager, ModuleServiceManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.{VfsUtilCore, VirtualFile}

/**
  * represents the Associated HPackage for an Module
  */
@State(
  name = "HaskforceHPackageModule",
  storages = Array(
    new Storage(StoragePathMacros.MODULE_FILE)
  )
)
//TODO make optPackage private after implementing #308, getPackage should then just return the optPackage
class HPackageModule(var optPackage: Option[HPackage], module: Module) extends PersistentStateComponent[PersistentStateWrapper] {
  def this(module: Module) = this(None, module)

  override def loadState(state: PersistentStateWrapper): Unit = {
    this.synchronized {
      optPackage = state.getState
        .flatMap(tuple => {
          val (location, state) = tuple
          HPackageManager.getInstance(module.getProject).loadPackage(state, location)
        })
    }
  }

  override def getState: PersistentStateWrapper = {
    optPackage.map(pkg => new PersistentStateWrapper(pkg.getState, FileUtil.toRelativePath(module.getProject, pkg.getLocation)))
      .getOrElse(new PersistentStateWrapper())
  }

  /**
    * returns the associated Package, falling back to the mainPackage if initialized
    */
  def getPackage: Option[HPackage] = {
    optPackage.orElse {
      HPackageManager.getInstance(module.getProject).mainPackage
    }
  }

  /**
    * removes the associated Package
    */
  def clearPackage(): Option[HPackage] = {
    this.synchronized {
      val oldOptional = optPackage
      optPackage = None
      oldOptional.foreach(pkg => pkg.emitEvent(Remove()))
      oldOptional
    }
  }

  /**
    * replaces the associates package with a new one
    * @param newPackage the new package to register
    * @return the old package if replaced, else None
    */
  def replacePackage(newPackage: HPackage): Option[HPackage] = {
    this.synchronized {
      val oldOptional = optPackage
      optPackage = Option(newPackage)
      if (oldOptional.isDefined) {
        val old: HPackage = oldOptional.get
        if (old.getPackageManager == newPackage.getPackageManager) {
          old.emitEvent(Update(newPackage))
        } else {
          old.emitEvent(Replace(newPackage))
        }
      }
      oldOptional
    }
  }
}

object HPackageModule {
  /**
    * returns the instance associated to the module
    */
  def getInstance(module: Module): HPackageModule = {
    ModuleServiceManager.getService(module, classOf[HPackageModule])
  }

  /**
    * returns the best matching module that also has an HPackage associated
    */
  def getInstance(virtualFile: VirtualFile, project: Project): Option[(Module, HPackage)] = {
    ModulesUtil.getMatchingModules(virtualFile, project)
      .flatMap(module => HPackageModule.getInstance(module).optPackage.map(pkg => (module, pkg)))
      .reduceOption((tuple1, tuple2) => {
        if (VfsUtilCore.isAncestor(tuple1._2.getLocation, tuple2._2.getLocation, false)) {
          tuple2
        } else {
          tuple1
        }
      })
  }

  /**
    * returns the best matching HPackage, falling back to main if not found
    */
  def getBestHPackage(virtualFile: VirtualFile, project: Project): Option[HPackage] = {
    ModulesUtil.getMatchingModules(virtualFile, project)
      .flatMap(module => HPackageModule.getInstance(module).optPackage.map(pkg => (module, pkg)))
      .reduceOption((tuple1, tuple2) => {
        if (VfsUtilCore.isAncestor(tuple1._2.getLocation, tuple2._2.getLocation, false)) {
          tuple2
        } else {
          tuple1
        }
      })
      .map(_._2)
      .orElse (HPackageManager.getInstance(project).mainPackage)
  }

  /**
    * returns a list of modules where that have an hPackage
    */
  def getModulesWithPackage(project: Project): List[Module] = {
    ModuleManager.getInstance(project).getModules
      .filter(module => HPackageModule.getInstance(module).optPackage.isDefined)
      .toList
  }
}
