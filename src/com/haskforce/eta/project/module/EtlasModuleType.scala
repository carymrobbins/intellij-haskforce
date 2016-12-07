package com.haskforce.eta.project.module

import javax.swing.Icon

import com.haskforce.HaskellIcons
import com.haskforce.macros.string.oneline
import com.intellij.ide.util.projectWizard.EmptyModuleBuilder
import com.intellij.openapi.module.ModuleType

final class EtlasModuleType extends ModuleType[EmptyModuleBuilder](EtlasModuleType.ID) {

  override def getName: String = "Etlas Module"

  override def getIcon: Icon = HaskellIcons.ETA_FILE

  override def getDescription: String = oneline(s"""
    Etlas modules are used for developing Haskell on the JVM using Eta
    and the Etlas Package Manager
  """)

  override def getNodeIcon(isOpened: Boolean): Icon = HaskellIcons.ETA_FILE

  override def createModuleBuilder(): EmptyModuleBuilder = new EmptyModuleBuilder
}

object EtlasModuleType {
  val ID = "ETLAS_MODULE"
  val INSTANCE = new EtlasModuleType
}
