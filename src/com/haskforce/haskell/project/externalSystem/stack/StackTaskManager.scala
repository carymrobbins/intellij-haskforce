package com.haskforce.haskell.project.externalSystem.stack

import java.util

import com.intellij.openapi.externalSystem.model.task.{ExternalSystemTaskId, ExternalSystemTaskNotificationListener}
import com.intellij.openapi.externalSystem.task.ExternalSystemTaskManager
import com.intellij.openapi.project.Project

final class StackTaskManager(
  project: Project
) extends ExternalSystemTaskManager[StackExecutionSettings] {

  override def executeTasks(
    id: ExternalSystemTaskId,
    taskNames: util.List[String],
    projectPath: String,
    settings: StackExecutionSettings,
    jvmAgentSetup: String,
    listener: ExternalSystemTaskNotificationListener
  ): Unit = {
    throw new NotImplementedError("StackTaskManager.executeTasks")
  }

  override def cancelTask(
    id: ExternalSystemTaskId,
    listener: ExternalSystemTaskNotificationListener
  ): Boolean = {
    throw new NotImplementedError("StackTaskManager.cancelTask")
  }
}
