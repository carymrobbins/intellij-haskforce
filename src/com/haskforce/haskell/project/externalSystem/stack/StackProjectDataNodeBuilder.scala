package com.haskforce.haskell.project.externalSystem.stack

import java.io.File

import com.haskforce.HaskellModuleType
import com.intellij.openapi.externalSystem.model.project.{ContentRootData, ExternalSystemSourceType, ModuleData, ProjectData}
import com.intellij.openapi.externalSystem.model.task.{ExternalSystemTaskId, ExternalSystemTaskNotificationListener}
import com.intellij.openapi.externalSystem.model.{DataNode, Key, ProjectKeys}

class StackProjectDataNodeBuilder(
  id: ExternalSystemTaskId,
  projectPath: String,
  settings: StackExecutionSettings,
  listener: ExternalSystemTaskNotificationListener
) {

  private def LOG(text: String, stdOut: Boolean = true): Unit = {
    listener.onTaskOutput(id, text + "\n", stdOut)
  }

  def create(): DataNode[ProjectData] = {
    LOG(s"rootProjectName=${settings.rootProjectName}")
    val projectDataNode = mkProjectDataNode(
      rootProjectName = settings.rootProjectName
    )
    settings.packageConfigAssocs.foreach { assoc =>
      LOG(s"packageConfigAssoc=$assoc")
      mkModuleDataNodes(
        packageDir = assoc.packageDir,
        packageConfig = assoc.packageConfig,
      ).foreach { moduleDataNode =>
        projectDataNode.addChild(moduleDataNode)
      }
    }
    projectDataNode
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
    externalName: String
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
    rootProjectName: String
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

  private def mkModuleDataNodes(
    packageDir: String,
    packageConfig: PackageConfig
  ): List[DataNode[ModuleData]] = {
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
    val maybeLibModuleDataNode = maybeLib.map(lib =>
      mkComponentModuleDataNode(
        packageDir = packageDir,
        packageConfig = packageConfig,
        component = lib
      )
    )
    val exeModuleDataNodes = exes.map { exe =>
      mkComponentModuleDataNode(
        packageDir = packageDir,
        packageConfig = packageConfig,
        component = exe
      )
    }
    maybeLibModuleDataNode.toList ++ exeModuleDataNodes
  }

  private def mkComponentModuleDataNode(
    packageDir: String,
    packageConfig: PackageConfig,
    component: PackageConfig.Component,
  ): DataNode[ModuleData] = {
    val id = mkComponentModuleId(packageConfig, component)
    val moduleDataNode =
      mkDataNode(
        ProjectKeys.MODULE,
        mkModuleData(
          id = id,
          externalName = id
        )
      )
    val contentRootData = mkContentRootData(packageDir)
    moduleDataNode.addChild(
      mkDataNode(
        ProjectKeys.CONTENT_ROOT,
        contentRootData
      )
    )
    val srcType = getComponentSourceType(component)
    component.hsSourceDirs.foreach { relSrcDir =>
      val canonicalSrcDir = new File(packageDir, relSrcDir).getCanonicalPath
      contentRootData.storePath(srcType, canonicalSrcDir)
    }
    moduleDataNode
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
