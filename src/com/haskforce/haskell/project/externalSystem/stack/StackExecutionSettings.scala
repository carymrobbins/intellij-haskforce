package com.haskforce.haskell.project.externalSystem.stack

import com.haskforce.settings.HaskellBuildSettings
import com.intellij.openapi.externalSystem.model.settings.ExternalSystemExecutionSettings
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Pair

/**
 * Contains the settings passed from the IDE to the external system
 * process which will perform the auto-import. In our case, we are using
 * an in-process external system so we can benefit from IDE features,
 * especially the PSI model. The out-of-process external system is mostly
 * for integrating with JVM build tools which may be leaky and affect the IDE.
 */
final case class StackExecutionSettings(
  project: Project,
  linkedProjectPath: String,
  stackExePath: String,
  stackYamlPath: String
) extends ExternalSystemExecutionSettings

object StackExecutionSettings {

  /** For use in [[StackManager.getExecutionSettingsProvider]] */
  def provider(project: Project, linkedProjectPath: String): StackExecutionSettings = {
    val buildSettings = HaskellBuildSettings.getInstance(project)
    // NOTE: The stackExePath and stackYamlPath should be configured at this
    // point by the StackProjectImporter. If this hasn't happened, we'll inform
    // the user with an external system exception.
    val stackExePath =
      Option(buildSettings.getStackPath).filter(_.nonEmpty).getOrElse {
        throw new StackSystemException("The Haskell 'stack' executable path is not set")

      }
    val stackYamlPath =
      Option(buildSettings.getStackFile).filter(_.nonEmpty).getOrElse {
        throw new StackSystemException("The Haskell 'stack.yaml' path is not set")
      }
    StackExecutionSettings(
      project = project,
      linkedProjectPath = linkedProjectPath,
      stackExePath = stackExePath,
      stackYamlPath = stackYamlPath
    )
  }

  def provider(pair: Pair[Project, String]): StackExecutionSettings = {
    provider(pair.first, pair.second)
  }
}
