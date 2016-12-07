package com.haskforce

import java.awt.event.ActionEvent
import java.awt.{Color, GridBagLayout}
import java.io.{File, IOException, PrintWriter}
import java.util
import java.util.concurrent.ExecutionException
import javax.swing._

import scala.collection.mutable
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.ide.util.projectWizard._
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.fileTypes.FileTypeManager
import com.intellij.openapi.module.{Module, ModuleType}
import com.intellij.openapi.options.ConfigurationException
import com.intellij.openapi.project.{Project, ProjectManager}
import com.intellij.openapi.projectRoots.SdkTypeId
import com.intellij.openapi.roots.ModifiableRootModel
import com.intellij.openapi.roots.ui.configuration.ModulesProvider
import com.intellij.openapi.ui.TextFieldWithBrowseButton
import com.intellij.openapi.util.Pair
import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.vfs.LocalFileSystem
import com.intellij.uiDesigner.core.Spacer
import org.apache.commons.lang.builder.HashCodeBuilder
import org.jetbrains.annotations.{NotNull, Nullable}
import com.haskforce.Implicits._
import com.haskforce.cabal.settings.{CabalComponentType, CabalPackageSettingsStep, CabalPackageTemplate}
import com.haskforce.cabal.settings.ui.NewCabalProjectForm
import com.haskforce.macros.string.dedent
import com.haskforce.settings.HaskellBuildSettings
import com.haskforce.ui.GC
import com.haskforce.utils.{GuiUtil, Logging}

/** Manages the creation of Haskell modules via interaction with the user. */
class HaskellModuleBuilder extends ModuleBuilder with SourcePathsBuilder with ModuleBuilderListener {

  val LOG = Logger.getInstance(getClass)

  /**
   * Hack to avoid this builder appearing in the top-level project wizard.
   * Prefer HaskellProjectTemplatesFactory
   */
  override def isAvailable: Boolean = false

  @throws(classOf[ConfigurationException])
  override def setupRootModel(rootModel: ModifiableRootModel) {
    addListener(this)
    setupRootModelCallbacks.foreach { _(rootModel) }
    if (rootModel.getSdk == null) rootModel.setSdk(HaskellSdkType.findOrCreateSdk())
    addContentEntries(rootModel)
    addExcludedRoots(rootModel)
  }

  private def addContentEntries(rootModel: ModifiableRootModel): Unit = {
    // Adapted from JavaModuleBuilder.setupRootModel
    lazy val localFS = LocalFileSystem.getInstance()
    Option(doAddContentEntry(rootModel)).foreach { contentEntry =>
      getSourcePaths.foreach { path =>
        val dir = new File(path.first)
        dir.mkdirs()
        Option(localFS.refreshAndFindFileByIoFile(dir)).foreach { sourceRoot =>
          // NOTE: The JavaModuleBuilder supplies 'path.second' as the third argument,
          // which corresponds to the 'packagePrefix'.  This doesn't seem to be applicable
          // to use, so let's omit it.
          contentEntry.addSourceFolder(sourceRoot, false)
        }
      }
    }
  }

  private def addExcludedRoots(rootModel: ModifiableRootModel): Unit = {
    rootModel.getContentEntries.foreach { contentEntry =>
      contentEntry.addExcludeFolder(contentEntry.getFile.getUrl + "/.stack-work")
    }
  }

  /**
   * Method provided so the HaskellCompilerToolsForm can tell us how to update the project settings.
   * TODO: This can probably be replaced with calls to '.addModuleConfigurationUpdater()'
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

  override def createWizardSteps
      (wizardContext: WizardContext,
       modulesProvider: ModulesProvider)
      : Array[ModuleWizardStep] = {
    if (wizardContext.isCreatingNewProject) {
      Array(
        HaskellBuildToolStep(this, wizardContext),
        HaskellCabalPackageSettingsStep(this, wizardContext)
      )
    } else {
      Array()
    }
  }

  @Nullable
  override def modifySettingsStep(@NotNull settingsStep: SettingsStep): ModuleWizardStep = {
    HaskellModifiedSettingsStep(this, settingsStep)
  }

  private[this] val sourcePaths = new util.ArrayList[Pair[String, String]]()

  override def setSourcePaths(paths: util.List[Pair[String, String]]): Unit = {
    sourcePaths.clear()
    sourcePaths.addAll(paths)
  }

  override def addSourcePath(info: Pair[String, String]): Unit = {
    sourcePaths.add(info)
  }

  override def getSourcePaths: util.List[Pair[String, String]] = sourcePaths

  override def hashCode: Int = HashCodeBuilder.reflectionHashCode(this)
}

case class HaskellBuildToolStep(
  moduleBuilder: HaskellModuleBuilder,
  wizardContext: WizardContext
) extends ModuleWizardStep {

  val form = new HaskellBuildToolStepForm(wizardContext)

  override def getComponent: JComponent = form.contentPane

  override def updateDataModel(): Unit = {
    moduleBuilder.registerSetupRootModelCallback { rootModel: ModifiableRootModel =>
      val project = rootModel.getProject
      val buildSettings = HaskellBuildSettings.getInstance(project)
      if (form.buildWithStackRadio.isSelected) {
        buildSettings.setUseStack(true)
        buildSettings.setStackPath(form.stackPathField.getText)
      } else if (form.buildWithCabalRadio.isSelected) {
        buildSettings.setUseCabal(true)
        buildSettings.setGhcPath(form.ghcPathField.getText)
        buildSettings.setCabalPath(form.cabalPathField.getText)
      } else {
        throw new RuntimeException("Expected Stack or Cabal build.")
      }
    }
  }

  override def validate(): Boolean = {
    // Clear any existing errors.
    List(
      form.stackPathErrorsField,
      form.ghcPathErrorsField,
      form.cabalPathErrorsField
    ).foreach { _.setText("") }

    var result = true
    if (form.buildWithStackRadio.isSelected) {
      if (!new File(form.stackPathField.getText).canExecute) {
        form.stackPathErrorsField.setText("Invalid stack path")
        result = false
      }
    } else if (form.buildWithCabalRadio.isSelected) {
      if (!new File(form.ghcPathField.getText).canExecute) {
        form.ghcPathErrorsField.setText("Invalid ghc path")
        result = false
      }
      if (!new File(form.cabalPathField.getText).canExecute) {
        form.cabalPathErrorsField.setText("Invalid cabal path")
        result = false
      }
    } else {
      form.stackPathErrorsField.setText("Must select Stack or Cabal build.")
      result = false
    }
    form.contentPane.revalidate()
    form.contentPane.repaint()
    result
  }
}

class HaskellBuildToolStepForm(wizardContext: WizardContext) {
  val buildWithRadioGroup = new ButtonGroup
  val buildWithStackRadio = new JRadioButton("Build with Stack")
  val buildWithCabalRadio = new JRadioButton("Build with Cabal")
  buildWithRadioGroup.add(buildWithStackRadio)
  buildWithRadioGroup.add(buildWithCabalRadio)
  val stackPathField = new TextFieldWithBrowseButton
  val stackPathErrorsField = new JLabel()
  stackPathErrorsField.setForeground(Color.red)
  GuiUtil.addFolderListener(stackPathField, "stack")
  val ghcPathField = new TextFieldWithBrowseButton
  val ghcPathErrorsField = new JLabel()
  ghcPathErrorsField.setForeground(Color.red)
  GuiUtil.addFolderListener(ghcPathField, "ghc")
  val cabalPathField = new TextFieldWithBrowseButton
  val cabalPathErrorsField = new JLabel()
  cabalPathErrorsField.setForeground(Color.red)
  GuiUtil.addFolderListener(cabalPathField, "cabal")

  private val stackFields = List(stackPathField)
  private val cabalFields = List(ghcPathField, cabalPathField)

  // Toggle fields enabled by build tool selected.
  buildWithStackRadio.addActionListener { e: ActionEvent =>
    stackFields.foreach { _.setEnabled(true) }
    cabalFields.foreach { _.setEnabled(false) }
  }
  buildWithCabalRadio.addActionListener { e: ActionEvent =>
    stackFields.foreach { _.setEnabled(false) }
    cabalFields.foreach { _.setEnabled(true) }
  }

  val contentPane = new JPanel(new GridBagLayout) {
    val gc = GC.pad(10, 5).northWest

    var gridY = 0
    add(buildWithStackRadio, gc.width(2).weight(1, 0).grid(0, gridY))

    gridY += 1
    add(new JLabel("Stack path:"), gc.grid(0, gridY))
    add(stackPathField, gc.fillHorizontal.grid(1, gridY))
    gridY += 1
    add(stackPathErrorsField, gc.fillHorizontal.grid(1, gridY))

    gridY += 1
    add(buildWithCabalRadio, gc.width(2).weight(x = 1).grid(0, gridY))

    gridY += 1
    add(new JLabel("GHC path:"), gc.grid(0, gridY))
    add(ghcPathField, gc.fillHorizontal.grid(1, gridY))
    gridY += 1
    add(ghcPathErrorsField, gc.fillHorizontal.grid(1, gridY))

    gridY += 1
    add(new JLabel("Cabal path:"), gc.grid(0, gridY))
    add(cabalPathField, gc.fillHorizontal.grid(1, gridY))
    gridY += 1
    add(cabalPathErrorsField, gc.fillHorizontal.grid(1, gridY))

    gridY += 1
    add(new Spacer, gc.grid(0, gridY).weight(y = 1))
  }

  // Populate from wizardContext
  private val project = Option(wizardContext.getProject).getOrElse(
    ProjectManager.getInstance.getDefaultProject
  )
  private val buildSettings = HaskellBuildSettings.getInstance(project)
  // Select the appropriate radio button, defaulting to Stack, and disable the other's fields.
  if (buildSettings.isCabalEnabled) {
    buildWithCabalRadio.setSelected(true)
    stackFields.foreach { _.setEnabled(false) }
  } else {
    buildWithStackRadio.setSelected(true)
    cabalFields.foreach { _.setEnabled(false) }
  }
  stackPathField.setText(buildSettings.getStackPath)
  ghcPathField.setText(buildSettings.getGhcPath)
  cabalPathField.setText(buildSettings.getCabalPath)
}

case class HaskellCabalPackageSettingsStep(
  moduleBuilder: HaskellModuleBuilder,
  wizardContext: WizardContext
) extends CabalPackageSettingsStep with Logging {

  val form = new NewCabalProjectForm

  override protected def updateModule(module: Module, rootModel: ModifiableRootModel): Unit = {
    super.updateModule(module, rootModel)
    if (wizardContext.isCreatingNewProject) runStackInitIfEnabled(rootModel.getProject)
  }

  private def runStackInitIfEnabled(project: Project): Unit = {
    val buildSettings = HaskellBuildSettings.getInstance(project)
    if (buildSettings.isStackEnabled) {
      val command = new GeneralCommandLine(buildSettings.getStackPath, "init")
      command.withWorkDirectory(project.getBasePath)
      try {
        command.createProcess()
        val buildSettings = HaskellBuildSettings.getInstance(project)
        buildSettings.setStackFile(FileUtil.join(project.getBasePath, "stack.yaml"))
      } catch {
        case e@(_: ExecutionException | _: IOException) =>
          LOG.error("Error when running `stack init`", e)
      }
    }
  }
}

case class HaskellModifiedSettingsStep(
  moduleBuilder: HaskellModuleBuilder,
  settingsStep: SettingsStep
) extends ModuleWizardStep {

  init()

  // Not needed by the module builder.
  override def getComponent: JComponent = null

  override def updateDataModel(): Unit = {}

  private def init(): Unit = {
    setCompilerOutputDir()
    setSdk()
  }

  override def validate(): Boolean = {
    val projectName = settingsStep.getModuleNameField.getText
    if(!projectName.matches("[a-zA-Z0-9-]+")){
      throw new ConfigurationException("Project name can only contain letters, numbers and hyphens", "Invalid Project name");
    }
    super.validate()
  }

  private def setSdk(): Unit = {
    settingsStep.getContext.setProjectJdk(HaskellSdkType.findOrCreateSdk())
  }

  private def setCompilerOutputDir(): Unit = {
    val c = settingsStep.getContext
    if (c.isCreatingNewProject && c.isProjectFileDirectorySet) {
      c.setCompilerOutputDirectory(new File(c.getProjectFileDirectory, "dist").getPath)
    }
  }
}
