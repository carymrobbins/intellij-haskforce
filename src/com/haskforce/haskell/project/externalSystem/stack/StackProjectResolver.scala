package com.haskforce.haskell.project.externalSystem.stack

import com.haskforce.HaskellModuleType
import com.intellij.openapi.externalSystem.model.project.{ContentRootData, ExternalSystemSourceType, ModuleData, ProjectData}
import com.intellij.openapi.externalSystem.model.task.{ExternalSystemTaskId, ExternalSystemTaskNotificationListener}
import com.intellij.openapi.externalSystem.model.{DataNode, Key, ProjectKeys}
import com.intellij.openapi.externalSystem.service.project.ExternalSystemProjectResolver

final class StackProjectResolver
  extends ExternalSystemProjectResolver[StackExecutionSettings] {

  override def resolveProjectInfo(
    id: ExternalSystemTaskId,
    projectPath: String,
    isPreviewMode: Boolean,
    settings: StackExecutionSettings,
    listener: ExternalSystemTaskNotificationListener
  ): DataNode[ProjectData] = {
    val projectDataNode = mkProjectDataNode(
      rootProjectName = settings.rootProjectName,
      projectPath = projectPath
    )
    settings.packageConfigAssocs.foreach { assoc =>
      projectDataNode.addChild(
        mkPackageModuleDataNode(
          projectPath = projectPath,
          packageDir = assoc.packageDir,
          packageConfig = assoc.packageConfig,
        )
      )
    }
    projectDataNode
  }

  override def cancelTask(
    taskId: ExternalSystemTaskId,
    listener: ExternalSystemTaskNotificationListener
  ): Boolean = {
    // Task cancellation not supported. At the moment, however, this
    // should be pretty fast so cancellation shouldn't really be necessary.
    false
  }

  /**
   * Creates a DataNode with no parent. The parent should be set by
   * supplying the returned DataNode to DataNode.addChild() except for
   * the root project DataNode which leaves the parent as null.
   */
  private def mkDataNode[A](
    key: Key[A],
    data: A
  ): DataNode[A] = {
    new DataNode(key, data, null)
  }

  private def mkModuleData(
    id: String,
    externalName: String,
    projectPath: String
  ): ModuleData = {
    new ModuleData(
      id,
      StackManager.PROJECT_SYSTEM_ID,
      HaskellModuleType.MODULE_TYPE_ID,
      externalName,
      projectPath,
      projectPath
    )
  }

  private def mkContentRootData(
    rootPath: String
  ): ContentRootData = {
    new ContentRootData(StackManager.PROJECT_SYSTEM_ID, rootPath)
  }

  private def mkProjectDataNode(
    rootProjectName: String,
    projectPath: String
  ): DataNode[ProjectData] = {
    mkDataNode(
      ProjectKeys.PROJECT,
      new ProjectData(
        StackManager.PROJECT_SYSTEM_ID,
        rootProjectName,
        projectPath,
        projectPath
      )
    )
  }

  private def mkPackageModuleDataNode(
    projectPath: String,
    packageDir: String,
    packageConfig: PackageConfig
  ): DataNode[ModuleData] = {
    // Note that 'exes' here consists of exe, test, and/or bench.
    val (libs, exes) = packageConfig.components.partition(
      _.typ == PackageConfig.Component.Type.Library
    )
    val maybeLib = libs match {
      case Nil => None
      case List(lib) => Some(lib)
      case _ =>
        throw new StackSystemException(
          "Too many 'library' components for package",
          vars = List(
            "libraryCount" -> libs.length,
            "packageConfig" -> packageConfig.name,
            "packageDir" -> packageDir,
          )
        )
    }
    val moduleDataNode = mkMaybeLibPackageModuleDataNode(
      projectPath = projectPath,
      packageDir = packageDir,
      packageConfig = packageConfig,
      maybeLib = maybeLib
    )
    exes.foreach { exe =>
      mkComponentModuleDataNode(
        projectPath = projectPath,
        packageDir = packageDir,
        packageConfig = packageConfig,
        component = exe
      )
    }
    moduleDataNode
  }

  private def mkMaybeLibPackageModuleDataNode(
    projectPath: String,
    packageDir: String,
    packageConfig: PackageConfig,
    maybeLib: Option[PackageConfig.Component]
  ): DataNode[ModuleData] = {
    maybeLib match {
      case Some(lib) =>
        mkComponentModuleDataNode(
          projectPath = projectPath,
          packageDir = packageDir,
          packageConfig = packageConfig,
          component = lib
        )

      case None =>
        mkNoLibPackageModuleDataNode(
          projectPath = projectPath,
          packageDir = packageDir,
          packageConfig = packageConfig
        )
    }
  }

  private def mkNoLibPackageModuleDataNode(
    projectPath: String,
    packageDir: String,
    packageConfig: PackageConfig,
  ): DataNode[ModuleData] = {
    val id = packageConfig.name
    val moduleDataNode = mkDataNode(
      ProjectKeys.MODULE,
      mkModuleData(
        id = id,
        externalName = id,
        projectPath = projectPath
      )
    )
    moduleDataNode.addChild(
      mkDataNode(
        ProjectKeys.CONTENT_ROOT,
        mkContentRootData(packageDir)
      )
    )
    moduleDataNode
  }

  private def mkComponentModuleDataNode(
    projectPath: String,
    packageDir: String,
    packageConfig: PackageConfig,
    component: PackageConfig.Component,
  ): DataNode[ModuleData] = {
    val id = mkComponentModuleId(packageConfig, component)
    val exeDataNode =
      mkDataNode(
        ProjectKeys.MODULE,
        mkModuleData(
          id = id,
          externalName = id,
          projectPath = projectPath
        )
      )
    val contentRootData = mkContentRootData(packageDir)
    val srcType = getComponentSourceType(component)
    component.hsSourceDirs.foreach { srcDir =>
      contentRootData.storePath(srcType, srcDir)
    }
    exeDataNode
  }

  private def mkComponentModuleId(
    packageConfig: PackageConfig,
    component: PackageConfig.Component
  ): String = {
    component.typ match {
      case PackageConfig.Component.Type.Library =>
        s"${packageConfig.name}:lib"
      case PackageConfig.Component.Type.Executable =>
        s"${packageConfig.name}:exe:${component.name}"
      case PackageConfig.Component.Type.TestSuite =>
        s"${packageConfig.name}:test:${component.name}"
      case PackageConfig.Component.Type.Benchmark =>
        s"${packageConfig.name}:bench:${component.name}"
    }
  }

  private def getComponentSourceType(
    component: PackageConfig.Component
  ): ExternalSystemSourceType = {
    component.typ match {
      case PackageConfig.Component.Type.Library =>
        ExternalSystemSourceType.SOURCE
      case PackageConfig.Component.Type.Executable =>
        ExternalSystemSourceType.SOURCE
      case PackageConfig.Component.Type.TestSuite =>
        ExternalSystemSourceType.TEST
      case PackageConfig.Component.Type.Benchmark =>
        ExternalSystemSourceType.TEST
    }
  }
}
