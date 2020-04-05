package com.haskforce.cabal.settings

import javax.swing.JComponent

import com.haskforce.cabal.settings.ui.NewCabalProjectForm
import com.haskforce.utils.Logging
import com.intellij.ide.util.projectWizard.ModuleBuilder.ModuleConfigurationUpdater
import com.intellij.ide.util.projectWizard.{ModuleBuilder, ModuleWizardStep, WizardContext}
import com.intellij.openapi.module.Module
import com.intellij.openapi.roots.ModifiableRootModel

/**
 * Used by a Cabal-based [[ModuleBuilder]].
 * Creates .cabal and Setup.hs files via the updateModule protected method.
 * To add to or modify this behavior, you can extend that method.
 */
trait CabalPackageSettingsStep extends ModuleWizardStep with Logging {

  def moduleBuilder: ModuleBuilder

  def wizardContext: WizardContext

  def form: NewCabalProjectForm

  override def updateDataModel(): Unit = {
    moduleBuilder.addModuleConfigurationUpdater(moduleUpdater)
  }

  override def getComponent: JComponent = form.getContentPane

  protected def moduleUpdater: ModuleConfigurationUpdater = new ModuleConfigurationUpdater {
    override def update(module: Module, rootModel: ModifiableRootModel): Unit = {
      updateModule(rootModel)
    }
  }

  protected def updateModule(rootModel: ModifiableRootModel): Unit = {
    val project = rootModel.getProject
    if (wizardContext.isCreatingNewProject && form.shouldInitializeCabalPackage) {
      val baseDir = project.getBasePath
      val name = project.getName
      CabalPackageTemplate.createCabalFile(baseDir, name, form.getData)
      CabalPackageTemplate.createSetupFile(baseDir)
    }
  }
}
