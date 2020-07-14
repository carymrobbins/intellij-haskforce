package com.haskforce.haskell.project.externalSystem.stack

import java.io.File

import com.haskforce.settings.HaskellBuildSettings
import com.haskforce.utils.ExecUtil
import com.intellij.openapi.externalSystem.importing.ImportSpecBuilder
import com.intellij.openapi.externalSystem.model.DataNode
import com.intellij.openapi.externalSystem.model.project.ProjectData
import com.intellij.openapi.externalSystem.service.execution.ProgressExecutionMode
import com.intellij.openapi.externalSystem.service.project.manage.ExternalProjectsManagerImpl
import com.intellij.openapi.externalSystem.service.project.{ExternalProjectRefreshCallback, ProjectDataManager}
import com.intellij.openapi.externalSystem.util.ExternalSystemUtil
import com.intellij.openapi.progress.ProgressManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.ThrowableComputable

object StackProjectImporter {

  def importProject(
    project: Project,
    projectDirectory: String
  ): Unit = {
    if (!configureStack(projectDirectory, project)) return
    ExternalProjectsManagerImpl.getInstance(project).runWhenInitialized(() => {
      ExternalSystemUtil.ensureToolWindowInitialized(
        project,
        StackManager.PROJECT_SYSTEM_ID
      )
    })
    val projectSettings = StackProjectSettings.of(projectDirectory)
    val stackSettings: StackSettings =
      StackManager.getInstance(project).getSettingsProvider.fun(project)
    stackSettings.linkProject(projectSettings)

    // TODO: Not sure if this step is strictly necessary, cargo culted from GradleOpenProjectProvider.
    ExternalSystemUtil.refreshProject(
      projectDirectory,
      new ImportSpecBuilder(project, StackManager.PROJECT_SYSTEM_ID)
        .usePreviewMode()
        .use(ProgressExecutionMode.MODAL_SYNC)
    )

    ExternalSystemUtil.refreshProject(
      projectDirectory,
      new ImportSpecBuilder(project, StackManager.PROJECT_SYSTEM_ID)
        .callback(ImportCallback(projectDirectory, project))
    )
  }

  // Total cargo cult from 'GradleOpenProjectProvider.createFinalImportCallback'
  private final case class ImportCallback(
    projectDirectory: String,
    project: Project
  ) extends ExternalProjectRefreshCallback {

    override def onSuccess(externalProject: DataNode[ProjectData]): Unit = {
      if (externalProject == null) return
      importData(externalProject)
    }

    private def importData(externalProject: DataNode[ProjectData]): Unit = {
      ProjectDataManager.getInstance().importData(externalProject, project, false)
    }
  }

  /**
   * Attempt ot configure the stack exe and stack.yaml for the given project.
   */
  private def configureStack(
    projectDirectory: String,
    project: Project
  ): Boolean = {
    val s = HaskellBuildSettings.getInstance(project)
    val stackYamlFile = new File(projectDirectory, "stack.yaml")
    if (!stackYamlFile.isFile) return false
    val stackYamlPath = stackYamlFile.getCanonicalPath
    // When the default stack exe is not valid, attempt to implicitly
    // configure it. If we can't get a valid stack exe, bail out.
    if (!currentStackExeIsValid(s)
      && !implicitlyConfigureStackExe(project, s)) {
      return false
    }
    s.setStackFile(stackYamlPath)
    s.setUseStack(true)
    true
  }

  private def currentStackExeIsValid(s: HaskellBuildSettings): Boolean = {
    Option(s.getStackPath).exists(p => new File(p).canExecute)
  }

  private def implicitlyConfigureStackExe(
    project: Project,
    s: HaskellBuildSettings
  ): Boolean = {
    val go: ThrowableComputable[Option[String], Exception] = () => {
      Option(ExecUtil.locateExecutableByGuessing("stack"))
    }
    // Guard against 'Synchronous execution on EDT' errors.
    ProgressManager.getInstance().runProcessWithProgressSynchronously(
      go,
      "Locating stack executable",
      true,
      project
    ) match {
      case None => false
      case Some(p) => s.setStackPath(p); true
    }
  }
}
