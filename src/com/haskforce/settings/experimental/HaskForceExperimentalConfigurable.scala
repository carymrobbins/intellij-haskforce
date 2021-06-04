package com.haskforce.settings.experimental

import com.intellij.ide.util.PropertiesComponent
import com.intellij.openapi.options.ConfigurationException
import com.intellij.openapi.project.Project
import javax.swing.JComponent

/** Settings for experimental features in HaskForce. */
class HaskForceExperimentalConfigurable(
  project: Project
) extends AbstractHaskForceExperimentalSettingsConfigurable {

  import HaskForceExperimentalConfigurable._

  private val props = PropertiesComponent.getInstance(project)

  private var savedState = State.load(props)

  override def getId: String = ID

  override def getDisplayName: String = DISPLAY_NAME

  override def getHelpTopic: String = DISPLAY_NAME

  override def createComponent(): JComponent = mainPanel

  override def isModified: Boolean = {
    State.parse(this) != this.savedState
  }

  @throws[ConfigurationException]
  override def apply(): Unit = {
    this.savedState = validate()
    State.save(savedState, props)
  }

  override def reset(): Unit = {
    ghcPkgEnabled.setSelected(savedState.ghcPkgEnabled)
  }

  @throws[ConfigurationException]
  private def validate(): State = {
    State.parse(this)
  }
}

object HaskForceExperimentalConfigurable {

  // Do not change this value as it is used for decoding in 'State.load()'.
  val ID = "haskforce.experimental.settings"

  val DISPLAY_NAME = "HaskForce (Experimental)"

  def isGhcPkgEnabled(project: Project): Boolean = {
    State.load(PropertiesComponent.getInstance(project)).ghcPkgEnabled
  }

  /** Parsed state from HaskForceExperimentalSettingsConfigurable */
  final case class State(
    ghcPkgEnabled: Boolean
  )

  object State {

    sealed trait Field
    object Field {
      case object ghcPkgEnabled extends Field
    }

    val default: State = State(
      ghcPkgEnabled = false
    )

    def load(props: PropertiesComponent): State = {
      State(
        ghcPkgEnabled =
          props.getBoolean(s"$ID.ghcPkgEnabled", State.default.ghcPkgEnabled)
      )
    }

    def save(state: State, props: PropertiesComponent): Unit = {
      props.setValue(s"$ID.ghcPkgEnabled", state.ghcPkgEnabled.toString)
    }

    def parse(c: HaskForceExperimentalConfigurable): State = {
      State(
        c.ghcPkgEnabled.isSelected
      )
    }
  }
}
