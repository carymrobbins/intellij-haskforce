package com.haskforce.eta.run

import java.util

import com.haskforce.utils.JDOMExternalizable
import com.intellij.execution.Executor
import com.intellij.execution.configuration.{AbstractRunConfiguration, EnvironmentVariablesData}
import com.intellij.execution.configurations.{ConfigurationFactory, RunConfiguration, RunProfileState}
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.openapi.module.Module
import com.intellij.openapi.options.SettingsEditor
import com.intellij.openapi.project.Project
import org.jdom.Element

class EtlasRunConfiguration(project: Project, factory: ConfigurationFactory)
  extends AbstractRunConfiguration(project, factory) {

  override def getValidModules: util.Collection[Module] = null

  override def getConfigurationEditor: SettingsEditor[_ <: RunConfiguration]
    = new EtlasRunConfigurationEditor(this)

  override def getState(executor: Executor, env: ExecutionEnvironment): RunProfileState
    = new EtlasCommandLineState(env, this)

  override def readExternal(element: Element): Unit = {
    super.readExternal(element)
    configState = JDOMExternalizable[EtlasRunConfigurationState].readExternal(element)
  }

  override def writeExternal(element: Element): Unit = {
    JDOMExternalizable[EtlasRunConfigurationState].writeExternal(element, configState)
    super.writeExternal(element)
  }

  def getConfigState: EtlasRunConfigurationState = configState

  def updateConfigState(f: EtlasRunConfigurationState => EtlasRunConfigurationState): Unit = {
    configState = f(configState)
  }

  private var configState = EtlasRunConfigurationState(
    programArguments = "run",
    workingDirectory = project.getBasePath,
    vmArguments = "",
    environmentVariables = EnvironmentVariablesData.DEFAULT
  )
}

final case class EtlasRunConfigurationState(
  programArguments: String,
  workingDirectory: String,
  vmArguments: String,
  environmentVariables: EnvironmentVariablesData
)

object EtlasRunConfigurationState {

  implicit val jdomExt: JDOMExternalizable[EtlasRunConfigurationState]
    = JDOMExternalizable.derive[EtlasRunConfigurationState]
}
