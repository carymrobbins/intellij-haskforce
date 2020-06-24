package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.importing.{AbstractOpenProjectProvider, ImportSpecBuilder}
import com.intellij.openapi.externalSystem.model.DataNode
import com.intellij.openapi.externalSystem.model.project.ProjectData
import com.intellij.openapi.externalSystem.service.execution.ProgressExecutionMode
import com.intellij.openapi.externalSystem.service.project.manage.ExternalProjectsManagerImpl
import com.intellij.openapi.externalSystem.service.project.{ExternalProjectRefreshCallback, ProjectDataManager}
import com.intellij.openapi.externalSystem.util.ExternalSystemUtil
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile

class StackOpenProjectProvider extends AbstractOpenProjectProvider {

  override def isProjectFile(virtualFile: VirtualFile): Boolean = {
    // TODO: Do we need to be more robust; e.g. check for alternate stack.yaml files?
    !virtualFile.isDirectory && virtualFile.getName == "stack.yaml"
  }

  override def linkAndRefreshProject(
    projectDirectory: String,
    project: Project
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
        .callback(
          StackOpenProjectProvider.ImportCallback(
            projectDirectory,
            project
          )
        )
    )
  }
}

object StackOpenProjectProvider {

  // Total cargo cult from 'GradleOpenProjectProvider.createFinalImportCallback'
  private final case class ImportCallback(
    projectDirectory: String,
    project: Project
  ) extends ExternalProjectRefreshCallback {

    override def onSuccess(externalProject: DataNode[ProjectData]): Unit = {
      if (externalProject == null) return
      importData(externalProject)
    }

//    private def selectDataToImport(externalProject: DataNode[ProjectData]): Unit = {
//      val settings = StackSettings.getInstance(project)
//      // val showSelectiveImportDialog = settings.showSelectiveImportDialogOnInitialImport()
//      val app = ApplicationManager.getApplication
//      if (!showSelectiveImportDialog || app.isHeadlessEnvironment) return
//      app.invokeAndWait(() => {
//        val projectInfo = new InternalExternalProjectInfo(
//          StackManager.PROJECT_SYSTEM_ID,
//          projectDirectory,
//          externalProject
//        )
//        val dialog = new ExternalProjectDataSelectorDialog(project, projectInfo)
//        if (dialog.hasMultipleDataToSelect) {
//          dialog.showAndGet()
//          ()
//        } else {
//          Disposer.dispose(dialog.getDisposable)
//        }
//      })
//    }

    private def importData(externalProject: DataNode[ProjectData]): Unit = {
      ProjectDataManager.getInstance().importData(externalProject, project, false)
    }
  }
}
