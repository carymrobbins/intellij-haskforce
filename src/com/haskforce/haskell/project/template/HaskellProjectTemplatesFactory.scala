package com.haskforce.haskell.project.template

import javax.swing.Icon

import com.haskforce.HaskellIcons
import com.haskforce.eta.project.template.EtlasProjectTemplate
import com.intellij.ide.util.projectWizard.WizardContext
import com.intellij.platform.{ProjectTemplate, ProjectTemplatesFactory}

/** */
class HaskellProjectTemplatesFactory extends ProjectTemplatesFactory {

  override def createTemplates(s: String, wizardContext: WizardContext): Array[ProjectTemplate] = Array(
    new HaskellProjectTemplate,
    new EtlasProjectTemplate
  )

  override def getGroups: Array[String] = Array(HaskellProjectTemplatesFactory.getGroups)

  override def getGroupIcon(s: String): Icon = HaskellIcons.FILE
}

object HaskellProjectTemplatesFactory {
  def getGroups = "Haskell"
}
