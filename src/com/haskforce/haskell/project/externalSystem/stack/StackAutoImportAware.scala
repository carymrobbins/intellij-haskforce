package com.haskforce.haskell.project.externalSystem.stack

import java.io.File
import java.util

import com.haskforce.settings.HaskellBuildSettings
import com.intellij.openapi.externalSystem.ExternalSystemAutoImportAware
import com.intellij.openapi.project.Project

/**
  * Handle auto-import logic for stack projects.
  * Mostly adapted from GradleAutoImportAware
  */
object StackAutoImportAware extends ExternalSystemAutoImportAware {

  override def getAffectedExternalProjectPath(
    changedFileOrDirPath: String,
    project: Project
  ): String = {
    val file = new File(changedFileOrDirPath)
    if (file.isDirectory) return null

    // TODO: This is a hack
    if (file.getName == "stack.yaml") return changedFileOrDirPath

    // Get the project stack.yaml
    val stackFile = HaskellBuildSettings.getInstance(project).getStackFile
    // If we don't have a stack.yaml configured, don't auto-import.
    if (stackFile == null) return null

    // Check if the changed file is our configured stack.yaml.
    if (file.getCanonicalPath == new File(stackFile).getCanonicalPath) return stackFile
    // Assume a modified package.yaml/.cabal file requires auto-import.
    // TODO: We could check to ensure it's configured in the stack.yaml, but
    // this function is called often so we need to come up with some sort of
    // caching for it.
    if (file.getName == "package.yaml") return stackFile
    if (file.getName.endsWith(".cabal")) return stackFile
    null
  }

  override def getAffectedExternalProjectFiles(
    projectPath: String,
    project: Project
  ): util.List[File] = {
    // TODO: Hack?
    util.Arrays.asList(
      List(
        Option(new File(projectPath, "stack.yaml")),
        Option(HaskellBuildSettings.getInstance(project).getStackFile).map(new File(_))
      ).flatten[File].filter(_.exists()): _*
    )
  }
}
