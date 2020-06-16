package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.service.settings.AbstractImportFromExternalSystemControl
import com.intellij.openapi.externalSystem.util.ExternalSystemSettingsControl

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

  private def notImpl(name: String) = throw new NotImplementedError(s"${getClass.getSimpleName}.$name")

  override def onLinkedProjectPathChange(
    path: String
  ): Unit = {
    notImpl("onLinkedProjectPathChange")
  }

  override def createProjectSettingsControl(
    settings: StackProjectSettings
  ): ExternalSystemSettingsControl[StackProjectSettings] = {
    notImpl("createProjectSettingsControl")
  }

  override def createSystemSettingsControl(
    settings: StackSettings
  ): ExternalSystemSettingsControl[StackSettings] = {
    notImpl("createSystemSettingsControl")
  }
}

object StackSettingsControl {
  def default: StackSettingsControl = {
    // TODO: Hack?
    // val defaultProject = ProjectManager.getInstance().getDefaultProject
    // new StackSettingsControl(
    //   new StackSettings(defaultProject),
    //   StackProjectSettings("stack.yaml")
    // )
    throw new NotImplementedError("StackSettingsControl.default")
  }
}
