package com.haskforce.haskell.project.externalSystem.stack.ui

import com.haskforce.haskell.project.externalSystem.stack.StackManager
import com.intellij.openapi.externalSystem.service.settings.AbstractExternalSystemToolWindowCondition
import com.intellij.openapi.externalSystem.service.task.ui.AbstractExternalSystemToolWindowFactory
import com.intellij.openapi.project.Project

class StackToolWindowFactory
  extends AbstractExternalSystemToolWindowFactory(StackManager.PROJECT_SYSTEM_ID) {

  override def isApplicable(project: Project): Boolean = {
    StackToolWindowFactoryCondition.value(project)
  }
}

object StackToolWindowFactoryCondition
  extends AbstractExternalSystemToolWindowCondition(StackManager.PROJECT_SYSTEM_ID)
