package com.haskforce

import java.io.File
import javax.swing.JComponent

import com.haskforce.Implicits._
import com.haskforce.cabal.settings.AddCabalPackageOptions
import com.haskforce.cabal.settings.ui.{AddCabalPackageUtil, HaskellCompilerToolsForm, NewCabalProjectForm}
import com.haskforce.jps.model.HaskellBuildOptions
import com.haskforce.settings.HaskellBuildSettings
import com.haskforce.utils.CabalExecutor
import com.haskforce.utils.CabalExecutor.CabalExecutorError
import com.intellij.ide.projectWizard.ProjectSettingsStep
import com.intellij.ide.util.projectWizard._
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.fileTypes.FileTypeManager
import com.intellij.openapi.module.{Module, ModuleType}
import com.intellij.openapi.options.ConfigurationException
import com.intellij.openapi.projectRoots.SdkTypeId
import com.intellij.openapi.roots.ModifiableRootModel
import com.intellij.openapi.roots.ui.configuration.ModulesProvider
import com.intellij.openapi.ui.Messages
import org.apache.commons.lang.builder.HashCodeBuilder
import org.jetbrains.annotations.{NotNull, Nullable}

import scala.collection.mutable

/**
 * Manages the creation of Haskell modules via interaction with the user.
 */
class HaskellModuleBuilder extends JavaModuleBuilder with SourcePathsBuilder with ModuleBuilderListener {
  val LOG = Logger.getInstance(getClass)

  @throws(classOf[ConfigurationException])
  override def setupRootModel(rootModel: ModifiableRootModel) {
    addListener(this)
    setupRootModelCallbacks.foreach { _(rootModel) }
    super.setupRootModel(rootModel)
  }

  /**
   * Method provided so the HaskellCompilerToolsForm can tell us how to update the project settings.
   */
  def registerSetupRootModelCallback(callback: ModifiableRootModel => Unit): Unit = {
    setupRootModelCallbacks += callback
  }
  val setupRootModelCallbacks = new mutable.MutableList[ModifiableRootModel => Unit]()

  /**
   * Returns the Haskell module type.
   */
  override def getModuleType: ModuleType[_ <: ModuleBuilder] = HaskellModuleType.getInstance

  /**
   * Ensures that SDK type is a Haskell SDK.
   */
  override def isSuitableSdkType(sdkType: SdkTypeId): Boolean = sdkType == HaskellSdkType.getInstance

  /**
   * Called after module is created.
   */
  def moduleCreated(@NotNull module: Module) {
    val haskellIgnoredList = Set("*.dyn_hi", "*.dyn_hi", "*.dyn_o", "*.hi", "*.o")
    val fileTypeManager = FileTypeManager.getInstance
    val newIgnoredList = fileTypeManager.getIgnoredFilesList.split(';').toSet.union(haskellIgnoredList)
    fileTypeManager.setIgnoredFilesList(newIgnoredList.mkString(";"))
  }


  override def createWizardSteps(wizardContext: WizardContext, modulesProvider: ModulesProvider) = Array(
      Some(HaskellSdkSettingsStep(this, wizardContext)),
      wizardContext.isCreatingNewProject.option { HaskellToolsSettingsStep(this, wizardContext) },
      Some(HaskellCabalPackageSettingsStep(this, wizardContext))
  ).flatten

  @Nullable
  override def modifySettingsStep(@NotNull settingsStep: SettingsStep): ModuleWizardStep = {
    new HaskellModifiedSettingsStep(this, settingsStep)
  }

  override def hashCode: Int = HashCodeBuilder.reflectionHashCode(this)

  var maybeToolsForm: Option[HaskellCompilerToolsForm] = None
  var maybeCabalForm: Option[NewCabalProjectForm] = None
}

case class HaskellSdkSettingsStep(moduleBuilder: HaskellModuleBuilder, wizardContext: WizardContext)
extends ProjectJdkForModuleStep(wizardContext, HaskellSdkType.getInstance()) {
  override def updateDataModel(): Unit = {
    super.updateDataModel()
    moduleBuilder.setModuleJdk(getJdk)
    moduleBuilder.maybeToolsForm.foreach { _.onSdkChange(Option(getJdk)) }
  }
}

case class HaskellToolsSettingsStep(moduleBuilder: HaskellModuleBuilder, wizardContext: WizardContext)
extends ModuleWizardStep {
  val form = new HaskellCompilerToolsForm(moduleBuilder)
  moduleBuilder.maybeToolsForm = Some(form)
  override def getComponent: JComponent = form.getContentPane
  override def updateDataModel(): Unit = {}
  override def validate(): Boolean = {
    if (!super.validate()) return false
    // We should only validate the paths if they are set.
    moduleBuilder.maybeToolsForm.foreach { toolsForm =>
      Seq(
        toolsForm.getCabalPath -> "Cabal",
        toolsForm.getGhcPath -> "GHC",
        toolsForm.getGhcModPath -> "GHC Mod",
        toolsForm.getGhcModiPath -> "GHC Modi"
      ).foreach { case (maybePath, name) =>
        maybePath.filter(p => p.nonEmpty && !new File(p).canExecute).foreach { _ =>
          Messages.showErrorDialog(s"Invalid $name path", "Configuration Error")
          return false
        }
      }
    }
    true
  }
}

case class HaskellCabalPackageSettingsStep(moduleBuilder: HaskellModuleBuilder, wizardContext: WizardContext)
extends ModuleWizardStep {
  val form = new NewCabalProjectForm
  moduleBuilder.maybeCabalForm = Some(form)
  override def getComponent: JComponent = form.getContentPane
  override def updateDataModel(): Unit = {}
}

case class HaskellModifiedSettingsStep(moduleBuilder: HaskellModuleBuilder, settingsStep: SettingsStep)
extends ModuleWizardStep {
  setCompilerOutputDir()

  // Not needed by the module builder.
  override def getComponent: JComponent = null

  override def updateDataModel(): Unit = {
    moduleBuilder.maybeToolsForm.foreach { _.save() }
    moduleBuilder.maybeCabalForm.filter(_.shouldInitializeCabalPackage).foreach { cabalForm =>
      val cabalOptions = buildCabalOptions(cabalForm)
      for (ghcPath <- getGhcPath;
           cabal <- createCabalExecutor().right.toOption) {
        cabal.init(ghcPath, AddCabalPackageUtil.buildArgs(cabalOptions))
      }
    }
  }

  private def getBuildSetting(withForm: HaskellCompilerToolsForm => Option[String],
    withSettings: HaskellBuildSettings => Option[String]): Option[String] = {
    moduleBuilder.maybeToolsForm.flatMap(withForm) match {
      case value@Some(_) => value
      case None => Option(settingsStep.getContext.getProject).flatMap { project =>
        withSettings(HaskellBuildSettings.getInstance(project))
      }
    }
  }

  private def getGhcPath: Option[String] = {
    getBuildSetting(_.getGhcPath, s => Option(s.getGhcPath).filter(_ != HaskellBuildOptions.DEFAULT_GHC_PATH))
  }

  private def getCabalPath: Option[String] = {
    getBuildSetting(_.getCabalPath, s => Option(s.getCabalPath).filter(_ != HaskellBuildOptions.DEFAULT_CABAL_PATH))
  }

  private def createCabalExecutor(): Either[CabalExecutorError, CabalExecutor] = {
    getCabalPath match {
      case Some(cabalPath) => CabalExecutor.create(cabalPath, Option(moduleBuilder.getModuleFileDirectory))
      case None => Left(CabalExecutor.NotConfigured)
    }
  }

  private def buildCabalOptions(cabalForm: NewCabalProjectForm): AddCabalPackageOptions = {
    val packageName = settingsStep.getModuleNameField.getText
    val rootDir = moduleBuilder.getModuleFileDirectory
    AddCabalPackageUtil.buildOptions(None, cabalForm, None, packageName, rootDir)
  }

  private def setCompilerOutputDir(): Unit = {
    val c = settingsStep.getContext
    if (c.isCreatingNewProject && c.isProjectFileDirectorySet) {
      c.setCompilerOutputDirectory(new File(c.getProjectFileDirectory, "dist").getPath)
    }
  }
}
