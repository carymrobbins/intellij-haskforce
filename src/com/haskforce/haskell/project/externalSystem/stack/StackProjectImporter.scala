package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.importing.ImportSpecBuilder
import com.intellij.openapi.externalSystem.model.DataNode
import com.intellij.openapi.externalSystem.model.project.ProjectData
import com.intellij.openapi.externalSystem.service.execution.ProgressExecutionMode
import com.intellij.openapi.externalSystem.service.project.{ExternalProjectRefreshCallback, ProjectDataManager}
import com.intellij.openapi.externalSystem.service.project.manage.ExternalProjectsManagerImpl
import com.intellij.openapi.externalSystem.util.ExternalSystemUtil
import com.intellij.openapi.project.Project

object StackProjectImporter {

  def importProject(
    project: Project,
    projectDirectory: String
  ): Unit = {
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
}
