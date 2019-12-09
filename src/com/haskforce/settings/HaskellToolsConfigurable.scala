package com.haskforce.settings

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.ide.util.PropertiesComponent
import com.intellij.openapi.project.Project
import com.intellij.ui.TextAccessor
import javax.swing.{JButton, JCheckBox, JTextField}

final class HaskellToolsConfigurable(
  project: Project,
) extends HaskellToolsConfigurableBase(project) {

  private val props = PropertiesComponent.getInstance(project)

}

object HaskellToolsConfigurable {

  trait Property {
    def isModified: Boolean
    def saveState(): Unit
    def restoreState(): Unit
  }

  trait Versioned {
    def updateVersion(): Unit
  }

  class PropertyCheckBox(
    props: PropertiesComponent,
    toolKey: BooleanToolKeyWithDefault,
    field: JCheckBox
  ) extends Property {
    private[this] var oldValue: Boolean = toolKey.getValue(props)

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
    toolKey: ToolKey[_] with ToolKey.Setter[_],
    field: TextAccessor
  ) extends Property {

    private[this] var oldValue: String = props.getValue(toolKey.name)

    override def isModified: Boolean = field.getText == oldValue

    override def saveState(): Unit = {
      oldValue = field.getText
      props.setValue(toolKey.name, oldValue)
    }

    override def restoreState(): Unit = {
      field.setText(oldValue)
    }
  }

  class VersionField(
    getVersion: () => Option[String],
    field: JTextField
  ) extends Versioned {
    override def updateVersion(): Unit = {
      field.setText(getVersion().getOrElse(""))
    }
  }

  abstract class Tool(
    props: PropertiesComponent,
    command: String,
    pathProperty: PropertyTextField,
    flagsProperty: PropertyTextField,
    versionField: VersionField,
    autoFindButton: JButton,
  ) extends Property with Versioned {

  }
}
