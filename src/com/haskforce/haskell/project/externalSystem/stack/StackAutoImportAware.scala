package com.haskforce.haskell.project.externalSystem.stack

import java.io.File
import java.util

import com.intellij.openapi.externalSystem.ExternalSystemAutoImportAware
import com.intellij.openapi.project.Project
import com.intellij.util.containers.ContainerUtil

/**
  * Handle auto-import logic for stack projects.
  * Mostly adapted from GradleAutoImportAware
  */
class StackAutoImportAware extends ExternalSystemAutoImportAware {
  override def getAffectedExternalProjectPath(
    changedFileOrDirPath: String, project: Project
  ): String = {
    // TODO: Very hacky, what's the point of getAffectedExternalProjectFiles??
    val file = new File(changedFileOrDirPath)
    if (file.isDirectory) return null
    // TODO: Probably needs to figure out the configured stack.yaml name
    // This is good for 99% of use cases though
    if (file.getName == "stack.yaml" || file.getName == "package.yaml") {
      return project.getBasePath
    }
    null
  }

  override def getAffectedExternalProjectFiles(
    projectPath: String, project: Project
  ): util.List[File] = {
    val res = new util.ArrayList[File]()
    val optProjectSettings = Option(
      StackSettings.getInstance(project).getLinkedProjectSettings(projectPath)
    )

    // TODO: Not sure if we actually want project settings to have stack.yaml
    // or if that belongs in some sort of system-level settings.
    res.add(new File(
      projectPath,
      optProjectSettings.fold("stack.yaml")(_.stackYamlPath)
    ))

    optProjectSettings
      .fold(ContainerUtil.set(projectPath))(_.getModules)
      .forEach { modulePath =>
        res.add(new File(modulePath, "package.yaml"))
        ()
      }

    res
  }
}
