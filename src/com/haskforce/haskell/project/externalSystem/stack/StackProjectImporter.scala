package com.haskforce.haskell.project.externalSystem.stack

import java.io.File

import com.haskforce.settings.HaskellBuildSettings
import com.haskforce.settings.experimental.HaskForceExperimentalConfigurable
import com.haskforce.tooling.ghcPkg.{GhcPkgDumpExecutor, GhcPkgDumpProjectCacheService}
import com.haskforce.tooling.stack.PackageConfigCacheService
import com.haskforce.utils.ExecUtil
import com.intellij.execution.configurations.GeneralCommandLine
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
    val configuredStack =
      configureStack(projectDirectory, project).getOrElse(return)

    ExternalProjectsManagerImpl.getInstance(project).runWhenInitialized(() => {
      val _ = ExternalSystemUtil.ensureToolWindowContentInitialized(
        project,
        StackManager.PROJECT_SYSTEM_ID
      )
    })
    val projectSettings = StackProjectSettings.of(projectDirectory)
    val stackSettings: StackSettings =
      StackManager.getInstance(project).getSettingsProvider.fun(project)

    // Link the stack project to the IDE if it hasn't been already.
    if (stackSettings.getLinkedProjectSettings(projectSettings.getExternalProjectPath) == null) {
      stackSettings.linkProject(projectSettings)
    }

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
        .callback(ImportCallback(projectDirectory, project, configuredStack))
    )
  }

  private final case class ConfiguredStack(
    stackExePath: String,
    stackYamlPath: String
  )

  // Total cargo cult from 'GradleOpenProjectProvider.createFinalImportCallback'
  private final case class ImportCallback(
    projectDirectory: String,
    project: Project,
    configuredStack: ConfiguredStack
  ) extends ExternalProjectRefreshCallback {

    override def onSuccess(externalProject: DataNode[ProjectData]): Unit = {
      if (externalProject == null) return
      importData(externalProject)
      if (HaskForceExperimentalConfigurable.isGhcPkgEnabled(project)) {
        buildStackDeps(project, projectDirectory, configuredStack)
        loadGhcPkgCache(project, projectDirectory, configuredStack)
      }
    }

    private def importData(externalProject: DataNode[ProjectData]): Unit = {
      ProjectDataManager.getInstance().importData(externalProject, project, false)
    }
  }

  /**
   * Attempt to configure the stack exe and stack.yaml for the given project.
   */
  private def configureStack(
    projectDirectory: String,
    project: Project
  ): Option[ConfiguredStack] = {
    val s = HaskellBuildSettings.getInstance(project)
    val stackYamlFile = new File(projectDirectory, "stack.yaml")
    if (!stackYamlFile.isFile) return None
    val stackYamlPath = stackYamlFile.getCanonicalPath
    // When the default stack exe is not valid, attempt to implicitly
    // configure it. If we can't get a valid stack exe, bail out.
    implicitlyConfigureStackExe(project, s).map { stackExePath =>
      s.setStackFile(stackYamlPath)
      s.setUseStack(true)
      ConfiguredStack(
        stackExePath = stackExePath,
        stackYamlPath = stackYamlPath
      )
    }
  }

  private def validateStackExePath(s: HaskellBuildSettings): Option[String] = {
    Option(s.getStackPath).filter(p => new File(p).canExecute)
  }

  private def implicitlyConfigureStackExe(
    project: Project,
    s: HaskellBuildSettings
  ): Option[String] = {
    validateStackExePath(s).orElse {
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
        case None => None
        case res@Some(p) => s.setStackPath(p); res
      }
    }
  }

  private def buildStackDeps(
    project: Project,
    projectDirectory: String,
    configuredStack: ConfiguredStack
  ): Boolean = {

    val go: Runnable = () => {
      val c = new GeneralCommandLine(
        configuredStack.stackExePath, "--stack-yaml", configuredStack.stackYamlPath,
        "build", "--dependencies-only"
      )
      c.setWorkDirectory(projectDirectory)
      c.setRedirectErrorStream(true)
      val p = c.createProcess()
      val exitCode = p.waitFor()
      if (exitCode != 0) {
        throw new StackSystemException(
          s"Building stack dependencies failed with exit code $exitCode"
        )
      }
    }

    ProgressManager.getInstance().runProcessWithProgressSynchronously(
      go,
      "Building dependencies with stack",
      true,
      project
    )
  }

  private def loadGhcPkgCache(
    project: Project,
    projectDirectory: String,
    configuredStack: ConfiguredStack
  ): Unit = {
    val packageConfigAssocs =
      PackageConfigCacheService.getInstance(project).getPackageConfigAssocs
    val cachedPkgs = new GhcPkgDumpExecutor(
      projectDirectory, configuredStack.stackExePath, configuredStack.stackYamlPath
    ).run()
    val cacheService = GhcPkgDumpProjectCacheService.getInstance(project)
    cacheService.putPkgs(cachedPkgs)
    packageConfigAssocs.foreach { assoc =>
      assoc.packageConfig.components.foreach { component =>
        val depPkgs = component.dependencies.flatMap(d => cachedPkgs.first(d.name, d.version))
        component.hsSourceDirs.foreach { srcDir =>
          cacheService.addDependencyForSourcePath(
            new File(assoc.packageDir, srcDir).getCanonicalPath, depPkgs
          )
        }
      }
    }
  }
}
