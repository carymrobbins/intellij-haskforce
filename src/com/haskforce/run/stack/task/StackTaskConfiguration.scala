package com.haskforce.run.stack.task

import java.util

import com.haskforce.utils.JDOMExternalizable
import com.intellij.execution.Executor
import com.intellij.execution.configuration.{AbstractRunConfiguration, EnvironmentVariablesData}
import com.intellij.execution.configurations.{ConfigurationFactory, RunProfileState, RuntimeConfigurationException}
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import org.jdom.Element

class StackTaskConfiguration(project: Project, configFactory: ConfigurationFactory) extends
  AbstractRunConfiguration(project, configFactory) {

  private var configState = StackTaskConfigurationState("", EnvironmentVariablesData.DEFAULT)

  override def isBuildBeforeLaunchAddedByDefault: Boolean = false

  def getConfigState: StackTaskConfigurationState = configState

  def updateConfigState(f: StackTaskConfigurationState => StackTaskConfigurationState): Unit = {
    configState = f(configState)
  }

  override def getValidModules: util.Collection[Module] = null

  override def getConfigurationEditor: StackTaskConfigurationEditorForm =
    new StackTaskConfigurationEditorForm

  override def getState(executor: Executor, executionEnvironment: ExecutionEnvironment): RunProfileState = {
    new StackTaskCommandLineState(executionEnvironment, this)
  }

  override def getEnvs: util.Map[String, String] = {
    super.getEnvs
  }

  override def checkConfiguration(): Unit = {
    if (!Option(configState.task).exists(_.trim.nonEmpty)) {
      throw new RuntimeConfigurationException("Please specify a task")
    }
  }
  override def readExternal(element: Element): Unit = {
    super.readExternal(element)
    configState = JDOMExternalizable.readExternal[StackTaskConfigurationState](element)
  }

  override def writeExternal(element: Element): Unit = {
    JDOMExternalizable.writeExternal(element, configState)
    super.writeExternal(element)
  }
}

final case class StackTaskConfigurationState(
  task: String,
  environmentVariables: EnvironmentVariablesData
)

object StackTaskConfigurationState {
  implicit val jdomExt: JDOMExternalizable[StackTaskConfigurationState]
    = JDOMExternalizable.derive2(apply, unapply)
}
