package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.model.task.{ExternalSystemTaskId, ExternalSystemTaskNotificationListenerAdapter}
import com.intellij.openapi.externalSystem.service.notification.{ExternalSystemNotificationManager, NotificationCategory, NotificationData, NotificationSource}

class StackProjectImportNotificationListener extends ExternalSystemTaskNotificationListenerAdapter {

  override def onTaskOutput(
    id: ExternalSystemTaskId,
    text: String,
    stdOut: Boolean
  ): Unit = {
    val project = id.findProject()
    val notification = new NotificationData(
      "Stack Project Import",
      text,
      NotificationCategory.INFO,
      NotificationSource.PROJECT_SYNC
    )
    ExternalSystemNotificationManager.getInstance(project).showNotification(
      id.getProjectSystemId,
      notification
    )
  }
}
