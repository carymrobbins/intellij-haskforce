package com.haskforce.settings

import com.haskforce.utils.{ExecUtil, NotificationUtil}
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.ide.util.PropertiesComponent
import com.intellij.notification.NotificationType
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.options.ConfigurationException
import com.intellij.openapi.project.Project
import com.intellij.ui.{JBColor, TextAccessor}
import javax.swing.{JButton, JCheckBox, JComponent, JTextField}

final class HaskellToolsConfigurable(
  project: Project
) extends HaskellToolsConfigurableBase {

  private val LOG = Logger.getInstance(getClass)

  private val props = PropertiesComponent.getInstance(project)

  private val tools: List[Tool] = List(
    // stylish-haskell
    new Tool(
      toolKey = ToolKey.STYLISH_HASKELL,
      command = "stylish-haskell",
      mkPathProperty = new ToolPathField(_, stylishPath, stylishVersion),
      flagsField = stylishFlags,
      autoFindButton = stylishAutoFind
    ),

    // hlint
    new Tool(
      toolKey = ToolKey.HLINT,
      command = "hlint",
      mkPathProperty = new ToolPathField(_, hlintPath, hlintVersion),
      flagsField = hlintFlags,
      autoFindButton = hlintAutoFind
    ),

    // hindent
    new Tool(
      toolKey = ToolKey.HINDENT,
      command = "hindent",
      mkPathProperty = new ToolPathField(_, hindentPath, hindentVersion),
      flagsField = hindentFlags,
      autoFindButton = hindentAutoFind
    ),

    // hsdev
    new Tool(
      toolKey = ToolKey.HSDEV,
      command = "hsdev",
      mkPathProperty = new ToolPathField(
        _, hsdevPath, hsdevVersion,
        versionCliArgs = List("version", "--compiler"),
        versionPostProcess = _.replace('\r', ' ').replace('\n', ' ')
      ),
      flagsField = hsdevFlags,
      autoFindButton = hsdevAutoFind,
      extraPropertyFields = List(
        new PropertyCheckBox(ToolKey.HSDEV.ENABLED, hsdevEnabled),
        new TypedPropertyField(ToolKey.HSDEV.PORT, hsdevPort),
        new PropertyCheckBox(ToolKey.HSDEV.SPAWN_SERVER, hsdevSpawnServer),
        new TypedPropertyField(ToolKey.HSDEV.SCAN_TIMEOUT_SECONDS, hsdevScanTimeout),
        new TypedPropertyField(ToolKey.HSDEV.COMMAND_TIMEOUT_SECONDS, hsdevCommandTimeout)
      )
    ),

    // ghc-mod
    new Tool(
      toolKey = ToolKey.GHC_MOD,
      command = "ghc-mod",
      mkPathProperty = new ToolPathField(
        _, ghcModPath, ghcModVersion,
        versionCliArgs = List("version")
      ),
      flagsField = ghcModFlags,
      autoFindButton = ghcModAutoFind,
    ),

    // ghc-modi
    new Tool(
      toolKey = ToolKey.GHC_MODI,
      command = "ghc-modi",
      mkPathProperty = new ToolPathField(
        _, ghcModiPath, ghcModiVersion,
        versionCliArgs = List("version")
      ),
      flagsField = ghcModiFlags,
      autoFindButton = ghcModiAutoFind,
      extraPropertyFields = List(
        new TypedPropertyField(ToolKey.GHC_MODI.RESPONSE_TIMEOUT_MS, ghcModiResponseTimeout),
        new TypedPropertyField(ToolKey.GHC_MODI.KILL_IDLE_TIMEOUT_MS, ghcModiKillIdleTimeout)
      )
    )
  )

  override def getId: String = HaskellToolsConfigurable.HASKELL_TOOLS_ID

  override def getDisplayName: String = HaskellToolsConfigurable.HASKELL_TOOLS_ID

  override def createComponent(): JComponent = mainPanel

  override def isModified: Boolean = tools.exists(_.isModified)

  @throws[ConfigurationException]
  override def apply(): Unit = {
    tools.foreach(_.validate())
    ghcModLegacyInteractivePreSaveHook()
    tools.foreach(_.updateVersion())
    tools.foreach(_.saveState())
  }

  // Infer ghc-modi configuration from ghc-mod.
  private def ghcModLegacyInteractivePreSaveHook(): Unit = {
    if (ghcModPath.getText.isEmpty) return
    if (ghcModiPath.getText.nonEmpty) return
    ghcModiPath.setText(ghcModPath.getText)
    if (!ghcModiFlags.getText.contains("legacy-interactive")) {
      ghcModiFlags.setText("legacy-interactive")
    } else {
      ghcModiFlags.setText(
        List(ghcModiFlags.getText, "legacy-interactive").mkString(" ")
      )
    }
  }

  private trait Property {
    def isModified: Boolean
    def saveState(): Unit
    def restoreState(): Unit
  }

  private trait Versioned {
    def updateVersion(): Unit
  }

  private trait Validatable {
    @throws[ConfigurationException]
    def validate(): Unit
  }

  private class PropertyCheckBox(
    toolKey: BooleanToolKeyWithDefault,
    field: JCheckBox
  ) extends Property {

    private[this] var oldValue: Boolean = toolKey.getValue(props)
    field.setSelected(oldValue)

    override def isModified: Boolean = field.isSelected != oldValue

    override def saveState(): Unit = {
      oldValue = field.isSelected
      toolKey.setValue(props, oldValue)
    }

    override def restoreState(): Unit = {
      field.setSelected(oldValue)
    }
  }

  private class PropertyTextField(
    toolKey: ToolKey[String],
    val field: TextAccessor
  ) extends Property {

    private[this] var oldValue: String = props.getValue(toolKey.name)
    field.setText(oldValue)

    override def isModified: Boolean = field.getText != oldValue

    override def saveState(): Unit = {
      oldValue = field.getText
      props.setValue(toolKey.name, oldValue)
    }

    override def restoreState(): Unit = {
      field.setText(oldValue)
    }
  }

  private class TypedPropertyField[A](
    toolKey: CodecToolKeyWithDefault[A],
    val field: JComponent with TextAccessor
  ) extends Property with Validatable {

    private[this] var oldValue: String = props.getValue(toolKey.name)
    field.setText(oldValue)

    private val originalBackground = field.getBackground
    field.setInputVerifier { _ =>
      toolKey.parseString(field.getText) match {
        case Left(_) =>
          field.setBackground(JBColor.RED)
          false
        case Right(_) =>
          field.setBackground(originalBackground)
          true
      }
    }

    override def isModified: Boolean = field.getText != oldValue

    override def saveState(): Unit = {
      oldValue = field.getText
      props.setValue(toolKey.name, field.getText)
    }

    override def restoreState(): Unit = {
      field.setText(oldValue)
    }

    override def validate(): Unit = {
      if (field.getText.isEmpty) return
      toolKey.parseString(field.getText) match {
        case Left(e) =>
          LOG.warn("Invalid Haskell tools configuration", e)
          throw new ConfigurationException(e.getMessage, e, "Haskell tools")
        case Right(_) => // noop
      }
    }
  }

  private class ToolPathField(
    toolKey: ToolKey[Option[String]],
    val field: TextAccessor,
    versionField: JTextField,
    versionCliArgs: List[String] = List("--version"),
    versionPostProcess: String => String = identity
  ) extends Property with Versioned {

    private[this] var oldValue: String = props.getValue(toolKey.name)
    field.setText(oldValue)
    updateVersion()

    override def isModified: Boolean = field.getText != oldValue

    override def saveState(): Unit = {
      oldValue = field.getText
      props.setValue(toolKey.name, oldValue)
    }

    override def restoreState(): Unit = {
      field.setText(oldValue)
    }

    override def updateVersion(): Unit = {
      if (field.getText.isEmpty) {
        versionField.setText("")
      } else {
        ExecUtil.readCommandLine(
          new GeneralCommandLine(field.getText :: versionCliArgs: _*)
        ) match {
          case Right(s) => versionField.setText(versionPostProcess(s))
          case Left(e) =>
            NotificationUtil.displaySimpleNotification(
              NotificationType.ERROR, null, "HaskellTools", e.getMessage
            )
            versionField.setText("")
        }
      }
    }
  }

  private class Tool(
    toolKey: AbstractSimpleToolSettingsKey[_],
    command: String,
    mkPathProperty: PathToolKey => ToolPathField,
    flagsField: TextAccessor,
    autoFindButton: JButton,
    extraPropertyFields: List[Property] = Nil
  ) extends Property with Versioned with Validatable {

    private val pathProperty = mkPathProperty(toolKey.PATH)

    private val flagsProperty = new PropertyTextField(toolKey.FLAGS, flagsField)

    private val allPropertyFields: List[Property] = (
      pathProperty
        :: flagsProperty
        :: extraPropertyFields
      )

    override def isModified: Boolean = allPropertyFields.exists(_.isModified)

    override def saveState(): Unit = {
      allPropertyFields.foreach(_.saveState())
      toolKey match {
        case n: ToolKey.NotifyChanged => n.notifyChanged(project)
        case _ => // skip
      }
    }

    override def restoreState(): Unit = {
      allPropertyFields.foreach(_.restoreState())
    }

    override def updateVersion(): Unit = {
      pathProperty.updateVersion()
    }

    @throws[ConfigurationException]
    override def validate(): Unit = {
      // Skip validation if the tool is not configured
      if (pathProperty.field.getText.isEmpty) return
      allPropertyFields.foreach {
        case x: Validatable => x.validate()
        case _ => // noop
      }
    }
  }
}

object HaskellToolsConfigurable {
  val HASKELL_TOOLS_ID = "Haskell Tools"
}
