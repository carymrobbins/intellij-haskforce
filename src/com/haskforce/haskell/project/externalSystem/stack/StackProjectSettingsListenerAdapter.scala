package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.settings.{DelegatingExternalSystemSettingsListener, ExternalSystemSettingsListener}

class StackProjectSettingsListenerAdapter(
  listener: ExternalSystemSettingsListener[StackProjectSettings]
) extends DelegatingExternalSystemSettingsListener[StackProjectSettings](listener)
  with StackProjectSettingsListener
