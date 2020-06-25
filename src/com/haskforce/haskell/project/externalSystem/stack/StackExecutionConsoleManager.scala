package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.model.ProjectSystemId
import com.intellij.openapi.externalSystem.model.task.ExternalSystemTask
import com.intellij.openapi.externalSystem.service.execution.DefaultExternalSystemExecutionConsoleManager

class StackExecutionConsoleManager
  extends DefaultExternalSystemExecutionConsoleManager {

  override def getExternalSystemId: ProjectSystemId = {
    StackManager.PROJECT_SYSTEM_ID
  }

  override def isApplicableFor(task: ExternalSystemTask): Boolean = {
    task.getId.getProjectSystemId == StackManager.PROJECT_SYSTEM_ID
  }

// TODO: Do we need to do this? Partially cargo culted from GradleExecutionConsoleManager
//  override def attachExecutionConsole(
//    project: Project,
//    task: ExternalSystemTask,
//    env: ExecutionEnvironment,
//    processHandler: ProcessHandler
//  ): ExecutionConsole = {
//    val executionConsole: ConsoleView =
//      TextConsoleBuilderFactory.getInstance()
//        .createBuilder(project)
//        .getConsole
//    executionConsole.attachToProcess(processHandler)
//    task match {
//      case executeTask: ExternalSystemExecuteTaskTask =>
//        executionConsole.addMessageFilter()
//    }
//  }
}
