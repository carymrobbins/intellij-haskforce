package com.haskforce.settings.experimental

import com.intellij.ide.util.PropertiesComponent
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.options.ConfigurationException
import com.intellij.openapi.project.Project
import com.intellij.ui.JBColor
import javax.swing.JComponent
import scalaz.syntax.apply.ToApplyOps
import scalaz.{NonEmptyList, Validation, ValidationNel}

class HaskForceExperimentalSettingsConfigurable(
  project: Project
) extends AbstractHaskForceExperimentalSettingsConfigurable {

  import HaskForceExperimentalSettingsConfigurable._

  private val props = PropertiesComponent.getInstance(project)

  private var state = State.load(props)

  override def getId: String = ID

  override def getDisplayName: String = DISPLAY_NAME

  override def getHelpTopic: String = DISPLAY_NAME

  override def createComponent(): JComponent = mainPanel

  override def isModified: Boolean = {
    RawState.mk(this) != State.raw(state)
  }

  @throws[ConfigurationException]
  override def apply(): Unit = {
    this.state = validate()
    clearAllInputValidationErrors()
    State.save(state, props)
  }

  override def reset(): Unit = {
    RawState.reset(State.raw(state), this)
  }

  @throws[ConfigurationException]
  private def validate(): State = {
    State.parse(RawState.mk(this)).valueOr { es =>
      es.foreach { e =>
        inputsByName.get(e.field) match {
          case Some(input) => setInputValidationError(input, e.message)
          case None => LOG.warn(s"Unknown field: ${e.field}")
        }
      }
      throw new ConfigurationException(
        es.stream.map(e => s"${e.field}: ${e.message}").mkString("\n")
      )
    }
  }

  private val inputsByName: Map[State.Field, JComponent] = Map(
    State.Field.ghcPkgEnabled -> this.ghcPkgEnabled,
    State.Field.ghcPkgTimeoutMillis -> this.ghcPkgTimeoutMillis
  )

  private def setInputValidationError(input: JComponent, message: String): Unit = {
    input.setBackground(JBColor.RED)
    input.setToolTipText(message)
  }

  private def clearInputValidationError(input: JComponent): Unit = {
    input.setBackground(JBColor.background())
  }

  private def clearAllInputValidationErrors(): Unit = {
    inputsByName.values.foreach(clearInputValidationError)
  }
}

object HaskForceExperimentalSettingsConfigurable {

  private val LOG = Logger.getInstance(getClass)

  // Do not change this value as it is used for decoding in 'State.load()'.
  val ID = "haskforce.experimental.settings"

  val DISPLAY_NAME = "HaskForce Experimental Settings"

  /** Raw, unparsed state from HaskForceExperimentalSettingsConfigurable. */
  final case class RawState(
    ghcPkgEnabled: Boolean,
    ghcPkgTimeoutMillis: String
  )

  object RawState {
    def mk(c: HaskForceExperimentalSettingsConfigurable): RawState = {
      RawState(
        c.ghcPkgEnabled.isSelected,
        c.ghcPkgTimeoutMillis.getText
      )
    }

    def reset(raw: RawState, c: HaskForceExperimentalSettingsConfigurable): Unit = {
      c.ghcPkgEnabled.setSelected(raw.ghcPkgEnabled)
      c.ghcPkgTimeoutMillis.setText(raw.ghcPkgTimeoutMillis)
    }
  }

  /** Parsed state from HaskForceExperimentalSettingsConfigurable */
  final case class State(
    ghcPkgEnabled: Boolean,
    ghcPkgTimeoutMillis: Long
  )

  object State {

    sealed trait Field
    object Field {
      case object ghcPkgEnabled extends Field
      case object ghcPkgTimeoutMillis extends Field
    }

    val default: State = State(
      ghcPkgEnabled = false,
      ghcPkgTimeoutMillis = 5000
    )

    def load(props: PropertiesComponent): State = {
      State(
        ghcPkgEnabled =
          props.getBoolean(s"$ID.ghcPkgEnabled", State.default.ghcPkgEnabled),
        ghcPkgTimeoutMillis =
          props.getLong(s"$ID.ghcPkgTimeoutMillis", State.default.ghcPkgTimeoutMillis)
      )
    }

    def save(state: State, props: PropertiesComponent): Unit = {
      props.setValue(s"$ID.ghcPkgEnabled", state.ghcPkgEnabled.toString)
      props.setValue(s"$ID.ghcPkgTimeoutMillis", state.ghcPkgTimeoutMillis.toString)
    }

    final case class ParseFailure(
      field: Field,
      message: String
    )

    type Parsed[A] = ValidationNel[ParseFailure, A]

    def raw(state: State): RawState = {
      RawState(
        state.ghcPkgEnabled,
        state.ghcPkgTimeoutMillis.toString
      )
    }

    def parse(raw: RawState): Parsed[State] = {
      (validatePure(raw.ghcPkgEnabled)
        |@| validateLong(Field.ghcPkgTimeoutMillis, raw.ghcPkgTimeoutMillis)
      )(State.apply)
    }

    private def validatePure[A](a: A): Parsed[A] = Validation.success(a)

    private def validateLong(field: Field, s: String): Parsed[Long] = {
      Validation.fromTryCatchNonFatal(s.toLong).leftMap(_ =>
        NonEmptyList(
          ParseFailure(
            field = field,
            s"Invalid input for number: $s"
          )
        )
      )
    }
  }
}
