package com.haskforce.haskell.project.externalSystem.stack.ui

import com.haskforce.haskell.project.externalSystem.stack.StackManager
import com.intellij.openapi.externalSystem.service.task.ui.AbstractExternalSystemToolWindowFactory
import com.intellij.openapi.project.Project

class StackToolWindowFactory
  extends AbstractExternalSystemToolWindowFactory(StackManager.PROJECT_SYSTEM_ID) {

  override def isApplicable(project: Project): Boolean = {
    !StackManager.getInstance(project)
      .getSettingsProvider.fun(project)
      .getLinkedProjectsSettings.isEmpty
  }
}
