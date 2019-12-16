package com.haskforce.settings

import com.haskforce.utils.{ExecUtil, NotificationUtil}
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.ide.util.PropertiesComponent
import com.intellij.notification.NotificationType
import com.intellij.openapi.options.ConfigurationException
import com.intellij.openapi.project.Project
import com.intellij.ui.{JBColor, TextAccessor}
import javax.swing.{JButton, JCheckBox, JComponent, JLabel, JTextField}

final class HaskellToolsConfigurable(
  project: Project
) extends HaskellToolsConfigurableBase {

  import HaskellToolsConfigurable._

  private val props = PropertiesComponent.getInstance(project)

  private val tools: List[Tool] = List(
    // stylish-haskell
    new Tool(
      props,
      command = "stylish-haskell",
      pathProperty = new ToolPathField(
        props, ToolKey.STYLISH_HASKELL.PATH, stylishPath, stylishVersion
      ),
      flagsProperty = new PropertyTextField(
        props, ToolKey.STYLISH_HASKELL.FLAGS, stylishFlags
      ),
      autoFindButton = stylishAutoFind
    ),

    // hlint
    new Tool(
      props,
      command = "hlint",
      pathProperty = new ToolPathField(
        props, ToolKey.HLINT.PATH, hlintPath, hlintVersion
      ),
      flagsProperty = new PropertyTextField(
        props, ToolKey.HLINT.FLAGS, hlintFlags
      ),
      autoFindButton = hlintAutoFind
    ),

    // hindent
    new Tool(
      props,
      command = "hindent",
      pathProperty = new ToolPathField(
        props, ToolKey.HINDENT.PATH, hindentPath, hindentVersion
      ),
      flagsProperty = new PropertyTextField(
        props, ToolKey.HINDENT.FLAGS, hindentFlags
      ),
      autoFindButton = hindentAutoFind
    ),

    // hsdev
    new Tool(
      props,
      command = "hsdev",
      pathProperty = new ToolPathField(
        props, ToolKey.HSDEV.PATH, hsdevPath, hsdevVersion,
        versionCliArgs = List("version", "--compiler"),
        versionPostProcess = _.replace('\r', ' ').replace('\n', ' ')
      ),
      flagsProperty = new PropertyTextField(
        props, ToolKey.HSDEV.FLAGS, hsdevFlags
      ),
      autoFindButton = hsdevAutoFind,
      extraPropertyFields = List(
        new PropertyCheckBox(props, ToolKey.HSDEV.ENABLED, hsdevEnabled),
        new TypedPropertyField(props, ToolKey.HSDEV.PORT, hsdevPort),
        new PropertyCheckBox(props, ToolKey.HSDEV.SPAWN_SERVER, hsdevSpawnServer),
        new TypedPropertyField(props, ToolKey.HSDEV.SCAN_TIMEOUT_SECONDS, hsdevScanTimeout),
        new TypedPropertyField(props, ToolKey.HSDEV.COMMAND_TIMEOUT_SECONDS, hsdevCommandTimeout)
      )
    ),

    // ghc-mod
    new Tool(
      props,
      command = "ghc-mod",
      pathProperty = new ToolPathField(
        props, ToolKey.GHC_MOD.PATH, ghcModPath, ghcModVersion,
        versionCliArgs = List("version")
      ),
      flagsProperty = new PropertyTextField(
        props, ToolKey.GHC_MOD.FLAGS, ghcModFlags
      ),
      autoFindButton = ghcModAutoFind,
    ),

    // ghc-modi
    new Tool(
      props,
      command = "ghc-modi",
      pathProperty = new ToolPathField(
        props, ToolKey.GHC_MODI.PATH, ghcModiPath, ghcModiVersion,
        versionCliArgs = List("version")
      ),
      flagsProperty = new PropertyTextField(
        props, ToolKey.GHC_MODI.FLAGS, ghcModiFlags
      ),
      autoFindButton = ghcModiAutoFind,
      extraPropertyFields = List(
        new TypedPropertyField(props, ToolKey.GHC_MODI.RESPONSE_TIMEOUT_MS, ghcModiResponseTimeout),
        new TypedPropertyField(props, ToolKey.GHC_MODI.KILL_IDLE_TIMEOUT_MS, ghcModiKillIdleTimeout)
      )
    )
  )

  override def getId: String = HaskellToolsConfigurable.HASKELL_TOOLS_ID

  override def getDisplayName: String = HaskellToolsConfigurable.HASKELL_TOOLS_ID

  override def createComponent(): JComponent = mainPanel

  override def isModified: Boolean = {
    tools.foreach { tool =>
      if (tool.isModified) return true
    }
    false
  }

  @throws[ConfigurationException]
  override def apply(): Unit = {
    tools.foreach(_.validate())
    ghcModLegacyInteractivePreSaveHook()
    tools.foreach(_.updateVersion())
    tools.foreach(_.saveState())
  }

  // Infer ghc-modi configuration from ghc-mod.
  private def ghcModLegacyInteractivePreSaveHook(): Unit = {
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
}

object HaskellToolsConfigurable {

  val HASKELL_TOOLS_ID = "Haskell Tools"

  trait Property {
    def isModified: Boolean
    def saveState(): Unit
    def restoreState(): Unit
  }

  trait Versioned {
    def updateVersion(): Unit
  }

  trait Validatable {
    @throws[ConfigurationException]
    def validate(): Unit
  }

  class PropertyCheckBox(
    props: PropertiesComponent,
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

  class PropertyTextField(
    props: PropertiesComponent,
    toolKey: ToolKey[String],
    val field: TextAccessor
  ) extends Property {

    private[this] var oldValue: String = props.getValue(toolKey.name)
    field.setText(oldValue)

    override def isModified: Boolean = field.getText == oldValue

    override def saveState(): Unit = {
      oldValue = field.getText
      props.setValue(toolKey.name, oldValue)
    }

    override def restoreState(): Unit = {
      field.setText(oldValue)
    }
  }

  class TypedPropertyField[A](
    props: PropertiesComponent,
    toolKey: CodecToolKeyWithDefault[A],
    val field: JComponent with TextAccessor
  ) extends Property with Validatable {

    private[this] var oldValue: String = props.getValue(toolKey.name)
    field.setText(oldValue)

    private val originalBackground = field.getBackground
    field.setInputVerifier { _ =>
      toolKey.parseString(field.getText) match {
        case Left(_) =>
          field.setBackground(originalBackground)
          true
        case Right(_) =>
          field.setBackground(JBColor.RED)
          false
      }
    }

    override def isModified: Boolean = {
      field.getText != oldValue
    }

    override def saveState(): Unit = {
      oldValue = field.getText
      props.setValue(toolKey.name, field.getText)
    }

    override def restoreState(): Unit = {
      field.setText(oldValue)
    }

    override def validate(): Unit = {
      toolKey.parseString(field.getText) match {
        case Left(e) => throw new ConfigurationException(e.getMessage, e, "Haskell tools")
        case Right(_) => // noop
      }
    }
  }

  class ToolPathField(
    props: PropertiesComponent,
    toolKey: ToolKey[Option[String]],
    val field: TextAccessor,
    versionField: JTextField,
    versionCliArgs: List[String] = List("--version"),
    versionPostProcess: String => String = identity
  ) extends Property with Versioned {

    private[this] var oldValue: String = props.getValue(toolKey.name)
    field.setText(oldValue)
    updateVersion()

    override def isModified: Boolean = {
      field.getText != oldValue
    }

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

  class Tool(
    props: PropertiesComponent,
    command: String,
    pathProperty: ToolPathField,
    flagsProperty: PropertyTextField,
    autoFindButton: JButton,
    extraPropertyFields: List[Property] = Nil
  ) extends Property with Versioned with Validatable {

    private val allPropertyFields: List[Property] = (
         pathProperty
      :: flagsProperty
      :: extraPropertyFields
    )

    override def isModified: Boolean = {
      allPropertyFields.foreach { x =>
        if (x.isModified) return true
      }
      false
    }

    override def saveState(): Unit = {
      allPropertyFields.foreach(_.saveState())
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
