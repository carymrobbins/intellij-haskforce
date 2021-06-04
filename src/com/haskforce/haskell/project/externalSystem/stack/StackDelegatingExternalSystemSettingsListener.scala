package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.settings.{DelegatingExternalSystemSettingsListener, ExternalSystemSettingsListener}

/**
 * Allows us to create a [[StackProjectSettingsListener]] from a supplied
 * listener delegate. This is particularly useful for implementing
 * [[StackSettings.subscribe]].
 */
class StackDelegatingExternalSystemSettingsListener(
  delegate: ExternalSystemSettingsListener[StackProjectSettings]
) extends
    DelegatingExternalSystemSettingsListener[
      StackProjectSettings
    ](delegate)
  with StackProjectSettingsListener {

  override def onStackProjectSettingsChange(): Unit = {}
}
