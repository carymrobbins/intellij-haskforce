package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.model.DataNode
import com.intellij.openapi.externalSystem.model.project.ProjectData
import com.intellij.openapi.externalSystem.model.task.{ExternalSystemTaskId, ExternalSystemTaskNotificationListener}
import com.intellij.openapi.externalSystem.service.project.ExternalSystemProjectResolver

final class StackProjectResolver
  extends ExternalSystemProjectResolver[StackExecutionSettings] {

  override def resolveProjectInfo(
    id: ExternalSystemTaskId,
    projectPath: String,
    isPreviewMode: Boolean,
    settings: StackExecutionSettings,
    listener: ExternalSystemTaskNotificationListener
  ): DataNode[ProjectData] = {
    new StackProjectDataNodeBuilder(
      id = id,
      projectPath = projectPath,
      settings = settings,
      listener = listener
    ).create()
  }

  override def cancelTask(
    taskId: ExternalSystemTaskId,
    listener: ExternalSystemTaskNotificationListener
  ): Boolean = {
    // Task cancellation not supported. At the moment, however, this
    // should be pretty fast so cancellation shouldn't really be necessary.
    false
  }
}
