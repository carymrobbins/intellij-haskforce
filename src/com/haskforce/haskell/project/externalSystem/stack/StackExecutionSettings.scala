package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.model.settings.ExternalSystemExecutionSettings

final case class StackExecutionSettings(
  projectPath: String,
  stackExePath: String
) extends ExternalSystemExecutionSettings

