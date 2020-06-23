package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.model.settings.ExternalSystemExecutionSettings

// Note: All of the fields must be marshallable. For example, you cannot
// add a 'Project' field since 'ProjectImpl' is not marshallable..
final case class StackExecutionSettings(
  linkedProjectPath: String,
  stackExePath: String,
  stackYamlPath: String,
  rootProjectName: String,
  // TODO: Does this need to be Array? List or Vector would be nice to
  // get a derived equals() method.
  packageConfigAssocs: Array[PackageConfigAssoc]
) extends ExternalSystemExecutionSettings {

  override def equals(o: Any): Boolean = {
    o match {
      case x: StackExecutionSettings =>
        (
          linkedProjectPath == x.linkedProjectPath
            && stackExePath == x.stackExePath
            && stackYamlPath == x.stackYamlPath
            && rootProjectName == x.rootProjectName
            && packageConfigAssocs.toVector == x.packageConfigAssocs.toVector
        )
    }
  }
}
