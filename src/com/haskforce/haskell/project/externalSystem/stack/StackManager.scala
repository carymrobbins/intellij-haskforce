package com.haskforce.haskell.project.externalSystem.stack

import java.io.File
import java.util

import com.haskforce.{HaskForceRuntime, HaskellIcons}
import com.haskforce.settings.HaskellBuildSettings
import com.intellij.execution.configurations.SimpleJavaParameters
import com.intellij.ide.actions.OpenProjectFileChooserDescriptor
import com.intellij.openapi.externalSystem.model.{ExternalSystemException, ProjectSystemId}
import com.intellij.openapi.externalSystem.service.project.autoimport.CachingExternalSystemAutoImportAware
import com.intellij.openapi.externalSystem.service.ui.DefaultExternalSystemUiAware
import com.intellij.openapi.externalSystem.util.ExternalSystemApiUtil
import com.intellij.openapi.externalSystem.{ExternalSystemAutoImportAware, ExternalSystemManager, ExternalSystemUiAware}
import com.intellij.openapi.fileChooser.{FileChooserDescriptor, FileChooserDescriptorFactory}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Pair
import com.intellij.util.Function
import javax.swing.Icon

final class StackManager
  extends ExternalSystemManager[
    StackProjectSettings,
    StackProjectSettingsListener,
    StackSettings,
    StackLocalSettings,
    StackExecutionSettings
  ]
  with ExternalSystemUiAware
  with ExternalSystemAutoImportAware {

  override def getSystemId: ProjectSystemId = {
    StackManager.PROJECT_SYSTEM_ID
  }

  override val getSettingsProvider: Function[Project, StackSettings] = {
    project => {
      StackSettings.getInstance(project)
    }
  }

  override val getLocalSettingsProvider: Function[Project, StackLocalSettings] = {
    project => {
      StackLocalSettings.getInstance(project)
    }
  }

  override val getExecutionSettingsProvider: Function[
    Pair[Project, String],
    StackExecutionSettings
  ] = args => {
    val project = args.first
    val projectPath = args.second
    val projectBuildSettings = HaskellBuildSettings.getInstance(project)
    val stackExePath = projectBuildSettings.getStackPath
    if (stackExePath == null) {
      throw new ExternalSystemException(
        "The Haskell 'stack' executable path is not set"
      )
    }
    val stackYamlPath = projectBuildSettings.getStackFile
    if (stackYamlPath == null) {
      throw new ExternalSystemException(
        "The Haskell 'stack.yaml' path is not set"
      )
    }
    StackExecutionSettingsBuilder.forProject(project, projectPath).create()
  }

  override val getProjectResolverClass: Class[StackProjectResolver] = {
    classOf[StackProjectResolver]
  }

  override def getTaskManagerClass: Class[StackTaskManager] = {
    classOf[StackTaskManager]
  }

  override def getExternalProjectDescriptor: FileChooserDescriptor =
    new OpenProjectFileChooserDescriptor(true) // TODO: Is this right?

  override def enhanceRemoteProcessing(parameters: SimpleJavaParameters): Unit = {
    parameters.getClassPath.addAll(HaskForceRuntime.classPath)
  }

  private val autoImport = new CachingExternalSystemAutoImportAware(StackAutoImportAware)

  override def getAffectedExternalProjectPath(
    changedFileOrDirPath: String, project: Project
  ): String = {
    autoImport.getAffectedExternalProjectPath(changedFileOrDirPath, project)
  }

  override def getAffectedExternalProjectFiles(
    projectPath: String, project: Project
  ): util.List[File] = {
    autoImport.getAffectedExternalProjectFiles(projectPath, project)
  }

  override def getProjectRepresentationName(
    targetProjectPath: String,
    rootProjectPath: String
  ): String = {
    ExternalSystemApiUtil.getProjectRepresentationName(
      targetProjectPath,
      rootProjectPath
    )
  }

  override def getExternalProjectConfigDescriptor: FileChooserDescriptor = {
    FileChooserDescriptorFactory.createSingleFolderDescriptor()
  }

  override def getProjectIcon: Icon = {
    HaskellIcons.FILE
  }

  override def getTaskIcon: Icon = {
    DefaultExternalSystemUiAware.INSTANCE.getTaskIcon
  }
}

object StackManager {

  val PROJECT_SYSTEM_ID = new ProjectSystemId("HASKELL_STACK", "Stack")

  def getInstance(project: Project): StackManager = {
    ExternalSystemApiUtil.getManager(PROJECT_SYSTEM_ID) match {
      case m: StackManager => m
      case null =>
        throw new IllegalStateException(
          s"StackManager.getInstance: Failed to get the StackManager for project $project"
        )
      case o =>
        throw new IllegalStateException(
          s"StackManager.getInstance: Expected StackManager but got ${o.getClass}"
        )
    }
  }
}
