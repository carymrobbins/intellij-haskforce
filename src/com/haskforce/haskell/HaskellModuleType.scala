package com.haskforce.haskell

import javax.swing.Icon

import com.haskforce.haskell.ui.HaskellIcons
import com.intellij.openapi.module._
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.{VfsUtilCore, VirtualFile}

import scala.collection.JavaConverters._

object HaskellModuleType {
  val MODULE_TYPE_ID = "HASKELL_MODULE"

  def getInstance: HaskellModuleType = {
    ModuleTypeManager.getInstance().findByID(MODULE_TYPE_ID).asInstanceOf[HaskellModuleType]
  }

  def findModules(project: Project): java.util.Collection[Module] = {
    ModuleUtil.getModulesOfType(project, HaskellModuleType.getInstance)
  }

  //TODO review ModuleFile
  def findModule(location: VirtualFile, project: Project): Option[Module] = {
    ModuleUtil.getModulesOfType(project, HaskellModuleType.getInstance)
      .asScala.filter(module => VfsUtilCore.isAncestor(module.getModuleFile.getParent, location, false))
      .reduceOption((module1, module2) => {
        VfsUtilCore.isAncestor(module1.getModuleFile.getParent, module2.getModuleFile.getParent, true) match {
          case true => module2
          case false => module1
        }
      })
  }
}

class HaskellModuleType extends ModuleType[HaskellModuleBuilder](HaskellModuleType.MODULE_TYPE_ID) {
    def createModuleBuilder(): HaskellModuleBuilder = new HaskellModuleBuilder()
    def getName: String = "Haskell Module"
    def getDescription: String = "Haskell modules are used for developing <b>Haskell</b> applications."
    def getBigIcon: Icon = HaskellIcons.FILE
    def getNodeIcon(@Deprecated isOpened: Boolean) = HaskellIcons.FILE
}
