package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.components.{State, Storage, StoragePathMacros}
import com.intellij.openapi.externalSystem.settings.AbstractExternalSystemLocalSettings
import com.intellij.openapi.project.Project

@State(
  name = "StackLocalSettings",
  storages = Array(new Storage(StoragePathMacros.WORKSPACE_FILE))
)
final class StackLocalSettings(project: Project)
  extends AbstractExternalSystemLocalSettings(
    StackManager.PROJECT_SYSTEM_ID,
    project
  )
