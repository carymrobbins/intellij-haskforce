package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.model.settings.ExternalSystemExecutionSettings

/**
 * Contains the settings passed from the IDE to the external system
 * process which will perform the auto-import. We pre-compute the
 * [[packageConfigAssocs]] and supply it here because the external
 * system process cannot access IDE features such as PSI parsing, which
 * is something we need in order to inspect the Cabal and YAML files.
 *
 * We could, instead, run the auto-import in the IDE process by updating the
 * HASKELL_STACK.system.in.process value in our plugin.xml to be "true";
 * however, it's not clear that would really buy us much. Will likely only
 * do a refactor like that if there's a very clear benefit.
 *
 * Note: All of the fields must be marshallable so the instance can be
 * serialized and sent to the external system process. For example, you cannot
 * add a 'Project' field since 'ProjectImpl' is not marshallable. If you
 * mess this up, you will end up with an exception in the Build
 * console stating:
 *
 *    error marshalling arguments; nested exception is:
 *        java.io.NotSerializableException: path.to.Class
 */
final case class StackExecutionSettings(
  linkedProjectPath: String,
  stackExePath: String,
  stackYamlPath: String,
  rootProjectName: String,
  packageConfigAssocs: Array[PackageConfigAssoc]
) extends ExternalSystemExecutionSettings
