package com.haskforce

import com.intellij.openapi.module.Module
import com.intellij.openapi.module.ModuleType
import com.intellij.openapi.module.ModuleTypeManager
import com.intellij.openapi.module.ModuleUtil
import com.intellij.openapi.project.Project

import javax.swing.Icon

import com.intellij.openapi.vfs.VirtualFile

object HaskellModuleType {
  val MODULE_TYPE_ID = "HASKELL_MODULE"

  def getInstance: HaskellModuleType = {
    ModuleTypeManager.getInstance().findByID(MODULE_TYPE_ID).asInstanceOf[HaskellModuleType]
  }

  def findModules(project: Project): java.util.Collection[Module] = {
    ModuleUtil.getModulesOfType(project, HaskellModuleType.getInstance)
  }

  def findCabalFile(module: Module): Option[VirtualFile] = {
    for {
      moduleFile <- Option(module.getModuleFile)
      parent <- Option(moduleFile.getParent)
      children <- Option(parent.getChildren)
      result <- children.find(_.getExtension == "cabal")
    } yield result
  }
}

class HaskellModuleType extends ModuleType[HaskellModuleBuilder](HaskellModuleType.MODULE_TYPE_ID) {
    def createModuleBuilder(): HaskellModuleBuilder = new HaskellModuleBuilder()
    def getName: String = "Haskell Module"
    def getDescription: String = "Haskell modules are used for developing <b>Haskell</b> applications."
    def getBigIcon: Icon = HaskellIcons.FILE
    def getNodeIcon(@Deprecated isOpened: Boolean) = HaskellIcons.FILE
}
