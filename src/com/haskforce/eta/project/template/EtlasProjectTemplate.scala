package com.haskforce.eta.project.template

import javax.swing.Icon

import com.haskforce.HaskellIcons
import com.haskforce.eta.project.module.EtlasModuleBuilder
import com.intellij.ide.util.projectWizard.AbstractModuleBuilder
import com.intellij.openapi.ui.ValidationInfo
import com.intellij.platform.ProjectTemplate

/** Etlas template (Eta language) used by the new project wizard. */
class EtlasProjectTemplate extends ProjectTemplate {

  override def getName: String = "Etlas (Experimental)"

  override def getIcon: Icon = HaskellIcons.ETA_FILE

  override def getDescription: String = "Etlas-based Eta project (JVM)"

  override def validateSettings(): ValidationInfo = null

  override def createModuleBuilder(): AbstractModuleBuilder = new EtlasModuleBuilder
}
