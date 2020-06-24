package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.service.settings.AbstractImportFromExternalSystemControl
import com.intellij.openapi.externalSystem.util.ExternalSystemSettingsControl
import com.intellij.openapi.project.ProjectManager

class StackSettingsControl(
  stackSettings: StackSettings,
  stackProjectSettings: StackProjectSettings
) extends
    AbstractImportFromExternalSystemControl[
      StackProjectSettings,
      StackProjectSettingsListener,
      StackSettings
    ](
      StackManager.PROJECT_SYSTEM_ID,
      stackSettings,
      stackProjectSettings
    ) {

  override def onLinkedProjectPathChange(
    path: String
  ): Unit = {
    setLinkedProjectPath(path)
  }

  override def createProjectSettingsControl(
    settings: StackProjectSettings
  ): ExternalSystemSettingsControl[StackProjectSettings] = {
    new StackProjectSettingsControl(settings)
  }

  override def createSystemSettingsControl(
    settings: StackSettings
  ): ExternalSystemSettingsControl[StackSettings] = {
    new StackSystemSettingsControl
  }
}

object StackSettingsControl {
  def forDefaultProject(): StackSettingsControl = {
    val defaultProject = ProjectManager.getInstance().getDefaultProject
    new StackSettingsControl(
      new StackSettings(defaultProject),
      new StackProjectSettings()
    )
  }
}
