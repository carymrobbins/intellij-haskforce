package com.haskforce.haskell.project.externalSystem.stack

import java.io.File

import com.haskforce.tooling.hpack.PackageYamlQuery
import com.haskforce.utils.YAMLFileUtil
import com.intellij.openapi.externalSystem.model.project.ProjectData
import com.intellij.openapi.externalSystem.model.task.{ExternalSystemTaskId, ExternalSystemTaskNotificationListener}
import com.intellij.openapi.externalSystem.model.{DataNode, ExternalSystemException, ProjectKeys}
import com.intellij.openapi.externalSystem.service.project.ExternalSystemProjectResolver
import prelude._

final class StackProjectResolver extends ExternalSystemProjectResolver[StackExecutionSettings] {

  override def resolveProjectInfo(
    id: ExternalSystemTaskId,
    projectPath: String,
    isPreviewMode: Boolean,
    settings: StackExecutionSettings,
    listener: ExternalSystemTaskNotificationListener
  ): DataNode[ProjectData] = {
    val rootProjectName = inferRootProjectName(projectPath).valueOr(throw _)
    val projectDataNode: ProjectData = new ProjectData(
      StackManager.PROJECT_SYSTEM_ID,
      rootProjectName,
      projectPath,
      projectPath
    )
    new DataNode[ProjectData](
      ProjectKeys.PROJECT,
      projectDataNode,
      null
    )
  }

  private def inferRootProjectName(projectPath: String): Either[ExternalSystemException, String] = {
    val root = new File(projectPath)
    val rootPackageYaml = new File(root, "package.yaml")

    val ePackageYamlName =
      YAMLFileUtil.parseYaml(rootPackageYaml)
        .flatMap(PackageYamlQuery.getName)
        .leftMapNel

    def eRootDirName =
      Either.cond[Throwable, String](
        root.isDirectory,
        root.getName,
        new IllegalArgumentException(s"'${root.getPath}' is not a directory")
      ).leftMapNel

    ePackageYamlName.orElseAccum(eRootDirName).leftMap { xs =>
      new ExternalSystemException(
        "Failed to determine root project name, errors occurred:\n"
          + xs.toList.map(_.getMessage).mkString("\n")
      )
    }
  }

  override def cancelTask(
    taskId: ExternalSystemTaskId,
    listener: ExternalSystemTaskNotificationListener
  ): Boolean = ???
}

object StackProjectResolver {
  //private val LOG = Logger.getInstance(classOf[StackProjectResolver])
}
