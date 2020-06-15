package com.haskforce.haskell.project.externalSystem.stack

import java.io.File
import java.util

import com.haskforce.settings.HaskellBuildSettings
import com.intellij.execution.configurations.SimpleJavaParameters
import com.intellij.ide.actions.OpenProjectFileChooserDescriptor
import com.intellij.openapi.externalSystem.model.ProjectSystemId
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

  override def getExecutionSettingsProvider: Function[
    Pair[Project, String],
    StackExecutionSettings
  ] = args => {
    val project = args.first
    val linkedProjectPath = args.second
    val stackExePath = HaskellBuildSettings.getInstance(project).getStackPath
    StackExecutionSettings(project, linkedProjectPath, stackExePath)
  }

  override val getProjectResolverClass: Class[StackProjectResolver] =
    classOf[StackProjectResolver]

  override def getTaskManagerClass: Class[StackTaskManager] =
    classOf[StackTaskManager]

  override def getExternalProjectDescriptor: FileChooserDescriptor =
    new OpenProjectFileChooserDescriptor(true) // TODO: Is this right?

  override def enhanceRemoteProcessing(parameters: SimpleJavaParameters): Unit = ()

  private val autoImport = StackAutoImportAware

  override def getAffectedExternalProjectPath(
    changedFileOrDirPath: String, project: Project
  ): String = autoImport.getAffectedExternalProjectPath(changedFileOrDirPath, project)

  override def getAffectedExternalProjectFiles(
    projectPath: String, project: Project
  ): util.List[File] = autoImport.getAffectedExternalProjectFiles(projectPath, project)
}

object StackManager {
  val PROJECT_SYSTEM_ID = new ProjectSystemId("HASKELL_STACK")
}
