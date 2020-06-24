package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.service.settings.AbstractExternalProjectSettingsControl
import com.intellij.openapi.externalSystem.util.PaintAwarePanel

class StackProjectSettingsControl(stackProjectSettings: StackProjectSettings)
  extends
    AbstractExternalProjectSettingsControl[StackProjectSettings](
      stackProjectSettings
    ) {

  override def fillExtraControls(content: PaintAwarePanel, indentLevel: Int): Unit = {}

  override def isExtraSettingModified: Boolean = false

  override def resetExtraSettings(isDefaultModuleCreation: Boolean): Unit = {}

  override def applyExtraSettings(settings: StackProjectSettings): Unit = {}

  override def validate(settings: StackProjectSettings): Boolean = true
}
