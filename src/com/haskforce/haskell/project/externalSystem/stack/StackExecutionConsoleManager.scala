package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.execution.configurations.RunConfiguration
import com.intellij.execution.process.ProcessHandler
import com.intellij.execution.ui.ExecutionConsole
import com.intellij.openapi.actionSystem.AnAction
import com.intellij.openapi.externalSystem.execution.ExternalSystemExecutionConsoleManager
import com.intellij.openapi.externalSystem.model.ProjectSystemId
import com.intellij.openapi.externalSystem.model.task.ExternalSystemTask
import com.intellij.openapi.util.Key

class StackExecutionConsoleManager
  extends ExternalSystemExecutionConsoleManager[
    // TODO: Likely need to make subclasses of these type params for 'Stack'.
    RunConfiguration,
    ExecutionConsole,
    ProcessHandler
  ] {

  override def getExternalSystemId: ProjectSystemId = {
    StackManager.PROJECT_SYSTEM_ID
  }

  override def onOutput(
    executionConsole: ExecutionConsole,
    processHandler: ProcessHandler,
    text: String,
    processOutputType: Key[_]
  ): Unit = {
    notImpl("onOutput")
  }

  override def isApplicableFor(task: ExternalSystemTask): Boolean = {
    notImpl("isApplicableFor")
  }

  override def getRestartActions(consoleView: ExecutionConsole): Array[AnAction] = {
    notImpl("getRestartActions")
  }

  private def notImpl(name: String) = throw new NotImplementedError(s"${this.getClass.getSimpleName}.$name")
}
