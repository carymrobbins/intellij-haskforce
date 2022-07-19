package com.haskforce

import javax.swing.Icon

import com.intellij.openapi.module._
import com.intellij.openapi.project.Project

object HaskellModuleType {
  val MODULE_TYPE_ID = "HASKELL_MODULE"

  def getInstance: HaskellModuleType = {
    ModuleTypeManager.getInstance().findByID(MODULE_TYPE_ID).asInstanceOf[HaskellModuleType]
  }

  def findModules(project: Project): java.util.Collection[Module] = {
    ModuleUtil.getModulesOfType(project, HaskellModuleType.getInstance)
  }
}

class HaskellModuleType extends ModuleType[HaskellModuleBuilder](HaskellModuleType.MODULE_TYPE_ID) {
  def getName: String = "Haskell Module"
  def getDescription: String = "Haskell modules are used for developing <b>Haskell</b> applications."
  def getBigIcon: Icon = HaskellIcons.FILE
  def getNodeIcon(@Deprecated isOpened: Boolean): Icon = HaskellIcons.FILE

  def createModuleBuilder(): HaskellModuleBuilder = {
    new HaskellModuleBuilder()
  }
}
