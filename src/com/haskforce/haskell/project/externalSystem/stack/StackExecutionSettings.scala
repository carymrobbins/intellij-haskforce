package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.model.settings.ExternalSystemExecutionSettings
import com.intellij.openapi.project.Project

final case class StackExecutionSettings(
  project: Project,
  linkedProjectPath: String,
  stackExePath: String,
  stackYamlPath: String
) extends ExternalSystemExecutionSettings

