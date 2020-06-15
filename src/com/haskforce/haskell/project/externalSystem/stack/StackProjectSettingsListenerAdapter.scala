package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.service.project.manage.ExternalProjectsManager
import com.intellij.openapi.project.Project

class StackProjectSettingsListenerAdapter(
  project: Project
) extends StackProjectSettingsListener {

  def onStackProjectSettingsChange(): Unit = {
    ExternalProjectsManager.getInstance(project)
      .getExternalProjectsWatcher
      .markDirty(project.getBasePath)
  }
}
