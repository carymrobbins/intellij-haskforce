package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.service.project.manage.ExternalProjectsManager
import com.intellij.openapi.externalSystem.settings.ExternalSystemSettingsListenerAdapter
import com.intellij.openapi.project.Project

class StackProjectSettingsListenerAdapter(
  project: Project
) extends ExternalSystemSettingsListenerAdapter[StackProjectSettings]
  with StackProjectSettingsListener {

  def onStackProjectSettingsChange(): Unit = {
    ExternalProjectsManager.getInstance(project)
      .getExternalProjectsWatcher
      .markDirty(project.getBasePath)
  }
}
