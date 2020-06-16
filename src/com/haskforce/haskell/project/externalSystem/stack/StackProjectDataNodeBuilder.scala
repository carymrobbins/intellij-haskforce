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
    LOG(s"ModuleData: externalName=$externalName")
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
    val maybeLibModuleDataNode = maybeLib.toList.flatMap(lib =>
      mkComponentModuleDataNodes(
        packageDir = packageDir,
        packageConfig = packageConfig,
        component = lib
      )
    )
    val exeModuleDataNodes = exes.flatMap { exe =>
      mkComponentModuleDataNodes(
        packageDir = packageDir,
        packageConfig = packageConfig,
        component = exe
      )
    }
    maybeLibModuleDataNode ++ exeModuleDataNodes
  }

  /**
   * Creates a separate module data node per source dir.
   * The reason we need to do this is that we can't have multiple component
   * modules point to the same content root. For example -
   *
   * my-pkg:lib
   *  - hs-source-dirs: ./src
   * my-pkg:exe:my-exe
   *  - hs-source-dirs: ./app
   * my-pkg:test:my-test
   *  - hs-source-dirs: ./test ./it
   *
   * For this package, we'll create the following module hierarchy -
   *
   * my-pkg [.]
   *  - my-pkg:lib [./src]
   *  - my-pkg:exe:my-exe [./app]
   *  - my-pkg:test:my-test::test [./test]
   *  - my-pkg:test:my-test::it [./it]
   *
   * It's a little weird to have multiple modules for components with multiple
   * src dirs, but it's not entirely clear the best way to handle this in Intellij.
   *
   * You might be tempted to do something fancy like find the nearest ancestor
   * directory above all of the source dirs for a component and make that the
   * content root, but as you can see, that won't work for the example above.
   */
  private def mkComponentModuleDataNodes(
    packageDir: String,
    packageConfig: PackageConfig,
    component: PackageConfig.Component,
  ): List[DataNode[ModuleData]] = {
    val id0 = mkComponentModuleId(packageConfig, component)

    // We the component only has a single src dir, we can just use
    // the component id, e.g. 'my-pkg:lib'. Otherwise, we need
    // to disambiguate, e.g. 'my-pkg:lib::src', 'my-package:lib::app'
    val getId: String => String = {
      if (component.hsSourceDirs.toSet.size == 1) {
        (_: String) => id0
      } else {
        (relSrcDir: String) => s"$id0::$relSrcDir"
      }
    }

    val srcType = getComponentSourceType(component)
    component.hsSourceDirs.iterator.map { relSrcDir =>
      val moduleId = getId(relSrcDir)
      val moduleDataNode = mkDataNode(
        ProjectKeys.MODULE,
        mkModuleData(id = moduleId, externalName = moduleId)
      )
      val canonicalSrcDir = new File(packageDir, relSrcDir).getCanonicalPath
      val contentRootData = mkContentRootData(canonicalSrcDir)
      contentRootData.storePath(srcType, canonicalSrcDir)
      moduleDataNode.addChild(
        mkDataNode(ProjectKeys.CONTENT_ROOT, contentRootData)
      )
      moduleDataNode
    }.toList
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
