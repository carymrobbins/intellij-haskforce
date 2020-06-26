package com.haskforce.haskell.project.externalSystem.stack

import java.io.File

import com.haskforce.HaskellModuleType
import com.intellij.openapi.externalSystem.model.project.{ContentRootData, ExternalSystemSourceType, LibraryData, LibraryDependencyData, LibraryLevel, ModuleData, ProjectData}
import com.intellij.openapi.externalSystem.model.task.{ExternalSystemTaskId, ExternalSystemTaskNotificationListener}
import com.intellij.openapi.externalSystem.model.{DataNode, Key, ProjectKeys}
import com.intellij.openapi.roots.DependencyScope

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
    if (!hasRootPackageConfig()) {
      val projectModuleDataNode = mkDataNode(
        ProjectKeys.MODULE,
        mkModuleData(s"${settings.rootProjectName}:project")
      )
      val projectModuleContentRootData = mkContentRootData(settings.linkedProjectPath)
      projectModuleContentRootData.storePath(
        ExternalSystemSourceType.EXCLUDED,
        new File(settings.linkedProjectPath, ".stack-work").getCanonicalPath
      )
      projectModuleContentRootData.storePath(
        ExternalSystemSourceType.EXCLUDED,
        new File(settings.linkedProjectPath, "dist").getCanonicalPath
      )
      projectModuleDataNode.addChild(
        mkDataNode(ProjectKeys.CONTENT_ROOT, projectModuleContentRootData)
      )
      projectDataNode.addChild(projectModuleDataNode)
    }
    settings.packageConfigAssocs.foreach { assoc =>
      LOG(s"packageConfigAssoc=$assoc")
      projectDataNode.addChild(
        mkPackageModuleDataNode(
          packageDir = assoc.packageDir,
          packageConfig = assoc.packageConfig,
        )
      )
    }
    projectDataNode
  }

  private def hasRootPackageConfig(): Boolean = {
    val canonicalProjectPath = new File(projectPath).getCanonicalPath
    settings.packageConfigAssocs.exists(assoc =>
      new File(assoc.packageDir).getCanonicalPath == canonicalProjectPath
    )
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
    id: String
  ): ModuleData = {
    LOG(s"ModuleData: id=$id")
    new ModuleData(
      id,
      StackManager.PROJECT_SYSTEM_ID,
      HaskellModuleType.MODULE_TYPE_ID,
      id,
      projectPath,
      projectPath
    )
  }

  private def mkContentRootData(
    rootPath: String
  ): ContentRootData = {
    LOG(s"ContentRootData: rootPath=$rootPath")
    new ContentRootData(StackManager.PROJECT_SYSTEM_ID, rootPath)
  }

  private def mkProjectDataNode(
    rootProjectName: String
  ): DataNode[ProjectData] = {
    LOG(s"ProjectData: rootProjectName: $rootProjectName")
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
    packageDir: String,
    packageConfig: PackageConfig
  ): DataNode[ModuleData] = {
    val packageModuleData = mkModuleData(packageConfig.name)
    val packageModuleDataNode = mkDataNode(
      ProjectKeys.MODULE,
      packageModuleData
    )
    val packageModuleContentRootData = mkContentRootData(packageDir)
    packageModuleDataNode.addChild(
      mkDataNode(ProjectKeys.CONTENT_ROOT, packageModuleContentRootData)
    )
    packageModuleContentRootData.storePath(
      ExternalSystemSourceType.EXCLUDED,
      new File(packageDir, "dist").getCanonicalPath
    )
    packageModuleContentRootData.storePath(
      ExternalSystemSourceType.EXCLUDED,
      new File(packageDir, ".stack-work").getCanonicalPath
    )
    packageConfig.components.foreach { component =>
      component.hsSourceDirs.foreach { relSrcDir =>
        packageModuleContentRootData.storePath(
          getComponentSourceType(component),
          new File(packageDir, relSrcDir).getCanonicalPath
        )
      }
    }

    val deps = new java.util.HashMap[String, DependencyScope]
    packageConfig.components.foreach { component =>
      getDependencyType(component) match {
        case DependencyType.Compile =>
          // This may, intentionally, overwrite any TEST deps as COMPILE deps.
          component.dependencies.foreach { dep =>
            deps.put(dep, DependencyScope.COMPILE)
          }
        case DependencyType.Test =>
          // We don't want duplicate deps. Any already included in COMPILE
          // don't need to be re-included in TEST.
          component.dependencies.foreach { dep =>
            deps.putIfAbsent(dep, DependencyScope.TEST)
          }
      }
    }
    deps.forEach { (dep, scope) =>
      packageModuleDataNode.addChild(
        mkLibDepNode(packageModuleData, dep, scope)
      )
    }

    packageModuleDataNode
  }

  private def mkLibDepNode(
    ownerModule: ModuleData,
    name: String,
    dependencyScope: DependencyScope
  ): DataNode[LibraryDependencyData] = {
    val libData = new LibraryData(
      StackManager.PROJECT_SYSTEM_ID,
      name
    )
    val libDepData = new LibraryDependencyData(
      ownerModule,
      libData,
      LibraryLevel.MODULE
    )
    libDepData.setScope(dependencyScope)
    mkDataNode(
      ProjectKeys.LIBRARY_DEPENDENCY,
      libDepData
    )
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

  private def getDependencyType(
    component: PackageConfig.Component
  ): DependencyType = {
    component.typ match {
      case PackageConfig.Component.Type.Library =>
        DependencyType.Compile
      case PackageConfig.Component.Type.Executable =>
        DependencyType.Compile
      case PackageConfig.Component.Type.TestSuite =>
        DependencyType.Test
      case PackageConfig.Component.Type.Benchmark =>
        DependencyType.Test
    }
  }

  private sealed trait DependencyType
  private object DependencyType {
    case object Compile extends DependencyType
    case object Test extends DependencyType
  }
}
