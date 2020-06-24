package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.settings.{DelegatingExternalSystemSettingsListener, ExternalSystemSettingsListener}

// Cargo cult from 'DelegatingGradleSettingsListenerAdapter'
class StackDelegatingExternalSystemSettingsListener(
  delegate: ExternalSystemSettingsListener[StackProjectSettings]
) extends
    DelegatingExternalSystemSettingsListener[
      StackProjectSettings
    ](delegate)
  with StackProjectSettingsListener {
  override def onStackProjectSettingsChange(): Unit = {
    println("StackDelegatingExternalSystemSettingsListener.onStackProjectSettingsChange")
  }
}
