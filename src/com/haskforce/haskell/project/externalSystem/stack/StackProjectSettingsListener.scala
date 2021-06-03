package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.settings.ExternalSystemSettingsListener

trait StackProjectSettingsListener
  extends ExternalSystemSettingsListener[StackProjectSettings] {

  def onStackProjectSettingsChange(): Unit
}
