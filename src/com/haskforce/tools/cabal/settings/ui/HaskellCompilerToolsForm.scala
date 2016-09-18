package com.haskforce.tools.cabal.settings.ui

import java.io.File

import com.haskforce.haskell.HaskellModuleBuilder
import com.haskforce.Implicits._
import com.haskforce.jps.model.HaskellBuildOptions
import com.haskforce.settings.{HaskellBuildSettings, ToolKey}
import com.haskforce.utils.{ExecUtil, GuiUtil}
import com.intellij.ide.util.PropertiesComponent
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.DefaultProjectFactory
import com.intellij.openapi.projectRoots.Sdk
import com.intellij.openapi.roots.ModifiableRootModel
import com.intellij.ui.TextAccessor

/**
 * Simple form which provides the user with configurable tools at project creation time.
 * TODO: It would be nice to unify this with HaskellToolsConfigurable.
 */
class HaskellCompilerToolsForm(moduleBuilder: HaskellModuleBuilder) extends HaskellCompilerToolsFormBase {
  private val LOG = Logger.getInstance(getClass)
  private val defaultProject = DefaultProjectFactory.getInstance.getDefaultProject
  private val defaultBuildSettings = HaskellBuildSettings.getInstance(defaultProject)
  var lastAutoSetCabalPath: Option[String] = None
  var lastAutoSetGhcPath: Option[String] = None

  initializeFields()

  def getContentPane = contentPane

  def getCabalPath: Option[String] = getFieldText(cabalPath)
  def getGhcPath: Option[String] = getFieldText(ghcPath)
  def getGhcModPath: Option[String] = getFieldText(ghcModPath)
  def getGhcModiPath: Option[String] = getFieldText(ghcModiPath)

  def shouldSetCabalAsDefault: Boolean = cabalSetAsDefault.isSelected
  def shouldSetGhcAsDefault: Boolean = ghcSetAsDefault.isSelected
  def shouldSetGhcModAsDefault: Boolean = ghcModSetAsDefault.isSelected
  def shouldSetGhcModiAsDefault: Boolean = ghcModiSetAsDefault.isSelected

  def save(): Unit = {
    lazy val defaultToolSettings = PropertiesComponent.getInstance(defaultProject)
    def setToolPath(props: PropertiesComponent, key: ToolKey)(path: String): Unit = {
      props.setValue(key.pathKey, path)
    }

    // Set default settings if the respective "Set as Default" box is checked.
    getCabalPath.filter(_ => shouldSetCabalAsDefault).foreach(defaultBuildSettings.setCabalPath)
    getGhcPath.filter(_ => shouldSetGhcAsDefault).foreach(defaultBuildSettings.setGhcPath)
    getGhcModPath.filter(_ => shouldSetGhcModAsDefault).foreach(setToolPath(defaultToolSettings, ToolKey.GHC_MOD_KEY))
    getGhcModiPath.filter(_ => shouldSetGhcModiAsDefault).foreach(setToolPath(defaultToolSettings, ToolKey.GHC_MODI_KEY))

    moduleBuilder.registerSetupRootModelCallback { rootModel: ModifiableRootModel =>
      val project = rootModel.getProject
      val buildSettings = HaskellBuildSettings.getInstance(project)
      getCabalPath.foreach(buildSettings.setCabalPath)
      getGhcPath.foreach(buildSettings.setGhcPath)
      val toolSettings = PropertiesComponent.getInstance(project)
      getGhcModPath.foreach(setToolPath(toolSettings, ToolKey.GHC_MOD_KEY))
      getGhcModiPath.foreach(setToolPath(toolSettings, ToolKey.GHC_MODI_KEY))
    }
  }

  /**
   * Auto-sets the cabal and ghc paths if new ones are found and the user hasn't manually changed them.
   */
  def onSdkChange(maybeSdk: Option[Sdk]): Unit = {
    maybeSdk.nullMap(_.getHomePath).foreach { sdkHomePath =>
      Seq(
        "cabal" -> (canAutoSetCabalPath, autoSetCabalPath _),
        "ghc" -> (canAutoSetGhcPath, autoSetGhcPath _)
      ).foreach { case (name, (canAutoSet, autoSet)) =>
        if (canAutoSet) {
          val exe = new File(sdkHomePath, s"bin/$name")
          if (exe.canExecute) autoSet(exe.getAbsolutePath)
        }
      }
    }
  }

  // Provide some facilities for auto-setting paths.  We should check the `canAutoSet*` method before `autoSet*`.
  // The only exception is when initializing fields, that way we initialize the `lastAutoSet*` var.

  private def autoSetCabalPath(newCabalPath: String): Unit = {
    cabalPath.setText(newCabalPath)
    lastAutoSetCabalPath = Some(newCabalPath)
  }
  private def canAutoSetCabalPath: Boolean = lastAutoSetCabalPath == getCabalPath

  private def autoSetGhcPath(newGhcPath: String): Unit = {
    ghcPath.setText(newGhcPath)
    lastAutoSetGhcPath = Some(newGhcPath)
  }
  private def canAutoSetGhcPath: Boolean = lastAutoSetGhcPath == getGhcPath

  private def getFieldText(field: TextAccessor): Option[String] = Option(field.getText).filter(_.nonEmpty)

  private def initializeFields(): Unit = {
    initializeCabalPath()
    initializeGhcPath()
    initializeGhcModPath()
    initializeGhcModiPath()
    addListeners()
  }

  private def addListeners(): Unit = {
    Seq(
      cabalPath -> "Cabal",
      ghcPath -> "GHC",
      ghcModPath -> "GHC Mod",
      ghcModiPath -> "GHC Modi"
    ).foreach { case (p, n) => GuiUtil.addFolderListener(p, n) }
  }

  /**
   * Locate executables in the /opt directory.
   */
  private def findOptExecutable(name: String): Option[File] = {
    for {
      files <- Option(new File(s"/opt/$name").listFiles)
      // Using reverse to grab the latest version.
      executable <- files.reverse.headOption.map(new File(_, s"bin/$name")).filter(_.canExecute)
    } yield executable
  }

  private def initializeCabalPath(): Unit = {
    (
      Option(defaultBuildSettings.getCabalPath).filter(_ != HaskellBuildOptions.DEFAULT_CABAL_PATH)
      #:: Option(ExecUtil.locateExecutableByGuessing("cabal"))
      #:: findOptExecutable("cabal").map(_.getPath)
      #:: Stream()
    ).flatten.headOption.foreach(autoSetCabalPath)
  }

  private def initializeGhcPath(): Unit = {
    (
      Option(defaultBuildSettings.getGhcPath).filter(_ != HaskellBuildOptions.DEFAULT_GHC_PATH)
      #:: Option(ExecUtil.locateExecutableByGuessing("ghc"))
      #:: findOptExecutable("ghc").map(_.getPath)
      #:: Stream()
    ).flatten.headOption.foreach(autoSetGhcPath)
  }

  private def initializeGhcModPath(): Unit = {
    (
      Option(ToolKey.GHC_MOD_KEY.getPath(defaultProject))
      #:: Option(ExecUtil.locateExecutableByGuessing("ghc-mod"))
      #:: Stream()
    ).flatten.headOption.foreach(ghcModPath.setText)
  }

  private def initializeGhcModiPath(): Unit = {
    (
      Option(ToolKey.GHC_MODI_KEY.getPath(defaultProject))
      #:: Option(ExecUtil.locateExecutableByGuessing("ghc-modi"))
      #:: Stream()
    ).flatten.headOption.foreach(ghcModiPath.setText)
  }
}
