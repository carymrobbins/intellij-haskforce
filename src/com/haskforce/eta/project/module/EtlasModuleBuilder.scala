package com.haskforce.eta.project.module

import java.io.File
import javax.swing.JComponent

import com.haskforce.cabal.settings.CabalPackageSettingsStep
import com.haskforce.cabal.settings.ui.NewCabalProjectForm
import com.haskforce.eta.settings.EtaBuildSettings
import com.haskforce.utils.{ExecUtil, GuiUtil}
import com.intellij.ide.util.projectWizard.ModuleBuilder.ModuleConfigurationUpdater
import com.intellij.ide.util.projectWizard._
import com.intellij.openapi.module.{JavaModuleType, Module}
import com.intellij.openapi.roots.ModifiableRootModel
import com.intellij.openapi.roots.ui.configuration.ModulesProvider
import com.intellij.openapi.ui.{Messages, TextFieldWithBrowseButton}

/** Manages the creation of Eta modules via interaction with the user. */
class EtlasModuleBuilder extends JavaModuleBuilder {

  override def getModuleType = EtlasModuleType.INSTANCE

  override def createWizardSteps(wizardContext: WizardContext, modulesProvider: ModulesProvider): Array[ModuleWizardStep] = {
    if (wizardContext.isCreatingNewProject) {
      Array(
        EtlasStep(this, wizardContext)
      )
    } else {
      Array.empty
    }
  }

  override def modifySettingsStep(settingsStep: SettingsStep): ModuleWizardStep = {
    new EtaStep(this, settingsStep)
  }

  override def setupRootModel(rootModel: ModifiableRootModel): Unit = {
    super.setupRootModel(rootModel)
    addExcludedRoots(rootModel)
  }

  private def addExcludedRoots(rootModel: ModifiableRootModel): Unit = {
    rootModel.getContentEntries.foreach { contentEntry =>
      contentEntry.addExcludeFolder(contentEntry.getFile.getUrl + "/dist")
    }
  }

  private case class EtlasStep(
    moduleBuilder: EtlasModuleBuilder,
    wizardContext: WizardContext
  ) extends CabalPackageSettingsStep {
    val form = new NewCabalProjectForm
  }

  private class EtaStep(moduleBuilder: EtlasModuleBuilder, settingsStep: SettingsStep) extends ModuleWizardStep {

    private val javaStep = JavaModuleType.getModuleType.modifyProjectTypeStep(
      settingsStep, EtlasModuleBuilder.this
    )
    settingsStep.addSettingsField("Eta path:", etaPathField)
    settingsStep.addSettingsField("Eta-pkg path:", etaPkgPathField)
    settingsStep.addSettingsField("Etlas path:", etlasPathField)

    override def updateDataModel(): Unit = {
      javaStep.updateDataModel()
      moduleBuilder.addModuleConfigurationUpdater(new ModuleConfigurationUpdater {
        override def update(module: Module, rootModel: ModifiableRootModel): Unit = {
          val project = rootModel.getProject
          val buildSettings = EtaBuildSettings.getInstance(project)
          buildSettings.setEtaPath(etaPathField.getText)
          buildSettings.setEtaPkgPath(etaPkgPathField.getText)
          buildSettings.setEtlasPath(etlasPathField.getText)
        }
      })
    }

    override lazy val getComponent: JComponent = new JComponent {}

    override def disposeUIResources(): Unit = {
      super.disposeUIResources()
      javaStep.disposeUIResources()
    }

    override def validate(): Boolean = {
      var result = super.validate() && javaStep.validate()
      val errors = new scala.collection.mutable.ListBuffer[String]
      if (!new File(etaPathField.getText).canExecute) {
        errors += "Invalid Eta path: " + etaPathField.getText
        result = false
      }
      if (!new File(etaPkgPathField.getText).canExecute) {
        errors += "Invalid Eta-pkg path: " + etaPkgPathField.getText
        result = false
      }
      if (!new File(etlasPathField.getText).canExecute) {
        errors += "Invalid Etlas path: " + etlasPathField.getText
        result = false
      }
      if (errors.nonEmpty) Messages.showErrorDialog(errors.mkString("\n"), "Errors")
      result
    }

    private lazy val etaPathField = newPathField("eta")
    private lazy val etaPkgPathField = newPathField("eta-pkg")
    private lazy val etlasPathField = newPathField("etlas")

    private def newPathField(name: String): TextFieldWithBrowseButton = {
      val field = new TextFieldWithBrowseButton()
      GuiUtil.addFolderListener(field, name)
      Option(ExecUtil.locateExecutableByGuessing(name)).foreach { field.setText }
      field
    }
  }
}
