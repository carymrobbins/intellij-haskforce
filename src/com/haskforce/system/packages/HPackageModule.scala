package com.haskforce.system.packages

import com.haskforce.system.utils.ModulesUtil
import com.intellij.openapi.components._
import com.intellij.openapi.module.{Module, ModuleServiceManager}
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ModuleRootManager
import com.intellij.openapi.vfs.{VfsUtilCore, VirtualFile}

/**
  * represents the Associated HPackage for an Module
  */
@State(
  name = "haskforceModuleHPackage",
  storages = Array(new Storage(StoragePathMacros.MODULE_FILE))
)
class HPackageModule(var optPackage: Option[HPackage], module: Module) extends PersistentStateComponent[PersistentStateWrapper] {
  def this(module: Module) = this(None, module)

  override def loadState(state: PersistentStateWrapper): Unit = {
    this.synchronized {
      val roots: List[VirtualFile] = ModuleRootManager.getInstance(module).getContentRoots.toList
      optPackage = state.getState
        .flatMap(state => HPackageManager.getInstance(module.getProject).loadPackage(state, roots))
    }
  }

  override def getState: PersistentStateWrapper = {
    optPackage.map(pkg => new PersistentStateWrapper(pkg.getState))
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
      .flatMap(module => HPackageModule.getInstance(module).getPackage.map(pkg => (module, pkg)))
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
      .flatMap(module => HPackageModule.getInstance(module).getPackage.map(pkg => (module, pkg)))
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
}

/**
  * used for persisting the state
  */
@SerialVersionUID(32L)
class PersistentStateWrapper(hPackageState: HPackageState) extends Serializable {
  def this() = this(null)
  def getState: Option[HPackageState] = Option(hPackageState)
}
