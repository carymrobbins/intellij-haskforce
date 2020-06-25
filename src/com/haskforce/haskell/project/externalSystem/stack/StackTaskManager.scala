package com.haskforce.haskell.project.externalSystem.stack

import java.util

import com.intellij.openapi.externalSystem.model.task.{ExternalSystemTaskId, ExternalSystemTaskNotificationListener}
import com.intellij.openapi.externalSystem.task.ExternalSystemTaskManager

final class StackTaskManager
  extends ExternalSystemTaskManager[StackExecutionSettings] {

  override def executeTasks(
    id: ExternalSystemTaskId,
    taskNames: util.List[String],
    projectPath: String,
    settings: StackExecutionSettings,
    jvmAgentSetup: String,
    listener: ExternalSystemTaskNotificationListener
  ): Unit = {
    // TODO
  }

  override def cancelTask(
    id: ExternalSystemTaskId,
    listener: ExternalSystemTaskNotificationListener
  ): Boolean = {
    // TODO
    false
  }
}
