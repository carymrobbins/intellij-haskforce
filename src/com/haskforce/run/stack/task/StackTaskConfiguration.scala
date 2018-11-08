package com.haskforce.run.stack.task

import java.util

import com.intellij.execution.Executor
import com.intellij.execution.configuration.AbstractRunConfiguration
import com.intellij.execution.configurations.{ConfigurationFactory, RunProfileState, RuntimeConfigurationException}
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.openapi.components.PathMacroManager
import com.intellij.openapi.module.Module
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.JDOMExternalizerUtil
import org.jdom.Element

class StackTaskConfiguration(project: Project, configFactory: ConfigurationFactory) extends
  AbstractRunConfiguration(project, configFactory) {

  val TASK: String = "TASK"

  var task: String = _

  override def getValidModules: util.Collection[Module] = null

  override def getConfigurationEditor: StackTaskConfigurationEditorForm =
    new StackTaskConfigurationEditorForm

  override def getState(executor: Executor, executionEnvironment: ExecutionEnvironment): RunProfileState = {
    new StackTaskCommandLineState(executionEnvironment, this)
  }

  override def checkConfiguration(): Unit = {
    if (!Option(task).exists(_.trim.nonEmpty)) {
      throw new RuntimeConfigurationException("Please specify an executable")
    }
  }
  override def readExternal(element: Element): Unit = {
    PathMacroManager.getInstance(getProject).expandPaths(element)
    super.readExternal(element)
    task = JDOMExternalizerUtil.readField(element, TASK)
  }

  override def writeExternal(element: Element): Unit = {
    super.writeExternal(element)
    JDOMExternalizerUtil.writeField(element, TASK, task)
    PathMacroManager.getInstance(getProject).collapsePathsRecursively(element)
  }

  def setTask(task: String): Unit = {
    this.task = task
  }
}
