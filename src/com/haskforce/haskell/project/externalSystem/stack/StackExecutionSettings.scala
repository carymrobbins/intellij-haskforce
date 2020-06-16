package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.model.settings.ExternalSystemExecutionSettings

// Note: All of the fields must be marshallable. For example, you cannot
// add a 'Project' field since 'ProjectImpl' is not marshallable..
final case class StackExecutionSettings(
  linkedProjectPath: String,
  stackExePath: String,
  stackYamlPath: String,
  rootProjectName: String,
  packageConfigAssocs: Array[PackageConfigAssoc]
) extends ExternalSystemExecutionSettings
