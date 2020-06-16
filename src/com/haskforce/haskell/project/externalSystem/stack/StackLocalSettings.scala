package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.components.{State, Storage, StoragePathMacros}
import com.intellij.openapi.externalSystem.settings.AbstractExternalSystemLocalSettings
import com.intellij.openapi.project.Project

@State(
  name = "StackLocalSettings",
  storages = Array(new Storage(StoragePathMacros.WORKSPACE_FILE))
)
final class StackLocalSettings(project: Project)
  extends AbstractStackLocalSettings[StackLocalSettings.State](
    StackManager.PROJECT_SYSTEM_ID,
    project,
    new StackLocalSettings.State
  ) {
}

object StackLocalSettings {
  class State extends AbstractExternalSystemLocalSettings.State
}