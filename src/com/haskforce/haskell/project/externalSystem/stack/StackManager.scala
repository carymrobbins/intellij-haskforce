package com.haskforce.haskell.project.externalSystem.stack

import java.io.File
import java.net.URL
import java.util

import com.intellij.execution.configurations.SimpleJavaParameters
import com.intellij.ide.actions.OpenProjectFileChooserDescriptor
import com.intellij.openapi.externalSystem.model.ProjectSystemId
import com.intellij.openapi.externalSystem.service.project.autoimport.CachingExternalSystemAutoImportAware
import com.intellij.openapi.externalSystem.{ExternalSystemAutoImportAware, ExternalSystemManager}
import com.intellij.openapi.fileChooser.FileChooserDescriptor
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Pair
import com.intellij.util.Function

final class StackManager
  extends ExternalSystemManager[
    StackProjectSettings,
    StackProjectSettingsListener,
    StackSettings,
    StackLocalSettings,
    StackExecutionSettings
  ]
  with ExternalSystemAutoImportAware {

  override def getSystemId: ProjectSystemId =
    StackManager.PROJECT_SYSTEM_ID

  override val getSettingsProvider: Function[Project, StackSettings] =
    new StackSettings(_)

  override val getLocalSettingsProvider: Function[Project, StackLocalSettings] =
    new StackLocalSettings(_)

  override def getExecutionSettingsProvider
    : Function[Pair[Project, String], StackExecutionSettings]
    = _ => new StackExecutionSettings() // TODO: Do we need this?

  override val getProjectResolverClass: Class[StackProjectResolver] =
    classOf[StackProjectResolver]

  override def getTaskManagerClass: Class[StackTaskManager] =
    classOf[StackTaskManager]

  override def getExternalProjectDescriptor: FileChooserDescriptor =
    new OpenProjectFileChooserDescriptor(true) // TODO: Is this right?

  override def enhanceRemoteProcessing(parameters: SimpleJavaParameters): Unit =
    () // TODO: What is this for?

  override def enhanceLocalProcessing(urls: util.List[URL]): Unit =
    () // TODO: What is this for?

  private val autoImport = new CachingExternalSystemAutoImportAware(
    new StackAutoImportAware
  )

  override def getAffectedExternalProjectPath(
    changedFileOrDirPath: String, project: Project
  ): String = autoImport.getAffectedExternalProjectPath(changedFileOrDirPath, project)

  override def getAffectedExternalProjectFiles(
    projectPath: String, project: Project
  ): util.List[File] = autoImport.getAffectedExternalProjectFiles(projectPath, project)
}

object StackManager {
  val PROJECT_SYSTEM_ID = new ProjectSystemId("STACK")
}
