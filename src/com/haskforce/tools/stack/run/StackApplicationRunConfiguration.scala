package com.haskforce.haskell.run.stack

import java.util

import com.haskforce.tools.stack.run.StackApplicationRunConfigurationEditorForm
import com.intellij.execution.Executor
import com.intellij.execution.configuration.AbstractRunConfiguration
import com.intellij.execution.configurations.{ConfigurationFactory, RunProfileState, RuntimeConfigurationException}
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.openapi.components.PathMacroManager
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.JDOMExternalizerUtil
import org.jdom.Element

class StackApplicationRunConfiguration(project: Project, configFactory: ConfigurationFactory) extends
  AbstractRunConfiguration(project, configFactory) {

  val EXECUTABLE: String = "EXECUTABLE"
  val EXECUTABLE_ARGUMENTS: String = "EXECUTABLE_ARGUMENTS"

  var executable: String = null
  var executableArguments: String = null

  override def getValidModules: util.Collection[Module] = null

  override def getConfigurationEditor: StackApplicationRunConfigurationEditorForm =
    new StackApplicationRunConfigurationEditorForm

  override def getState(executor: Executor, executionEnvironment: ExecutionEnvironment): RunProfileState = {
    return new StackRunCommandLineState(executionEnvironment, this)
  }

  override def checkConfiguration(): Unit = {
    if (!Option(executable).exists(_.trim.nonEmpty)) {
      throw new RuntimeConfigurationException("Please specify an executable")
    }
  }
  override def readExternal(element: Element): Unit = {
    PathMacroManager.getInstance(getProject).expandPaths(element)
    super.readExternal(element)
    executable = JDOMExternalizerUtil.readField(element, EXECUTABLE)
    executableArguments = JDOMExternalizerUtil.readField(element, EXECUTABLE_ARGUMENTS)
  }

  override def writeExternal(element: Element): Unit = {
    super.writeExternal(element)
    JDOMExternalizerUtil.writeField(element, EXECUTABLE, executable)
    JDOMExternalizerUtil.writeField(element, EXECUTABLE_ARGUMENTS, executableArguments)
    PathMacroManager.getInstance(getProject).collapsePathsRecursively(element)
  }

  def setExecutable(executable: String) {
    this.executable = executable
  }

  def setExecutableArguments(executableArguments: String) {
    this.executableArguments = executableArguments
  }
}
