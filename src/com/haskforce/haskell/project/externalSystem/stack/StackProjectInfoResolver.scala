package com.haskforce.haskell.project.externalSystem.stack

import java.io.{BufferedReader, File, InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets

import com.haskforce.HaskellModuleType
import com.haskforce.settings.experimental.HaskForceExperimentalConfigurable
import com.haskforce.tooling.ghcPkg.{GhcPkgDumpExecutor, GhcPkgDumpProjectCacheService}
import com.haskforce.tooling.hpack.PackageYamlQuery
import com.haskforce.utils.PsiFileParser
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.externalSystem.model.project._
import com.intellij.openapi.externalSystem.model.task.{ExternalSystemTaskId, ExternalSystemTaskNotificationListener}
import com.intellij.openapi.externalSystem.model.{DataNode, Key, ProjectKeys}
import com.intellij.openapi.roots.DependencyScope
import org.apache.commons.io.IOUtils
import org.jetbrains.yaml.psi.YAMLFile
import prelude._

class StackProjectInfoResolver(
  id: ExternalSystemTaskId,
  projectPath: String,
  settings: StackExecutionSettings,
  listener: ExternalSystemTaskNotificationListener,
  workManager: StackProjectResolver.WorkManager
) {

  private def LOG(text: String, stdOut: Boolean = true): Unit = {
    listener.onTaskOutput(id, text + "\n", stdOut)
  }

  // Runs in 'workManager.compute' to ensure that the thread is cancellable.
  // Any subprocesses should be spawned via the 'workManager' to ensure
  // they are cancellable.
  def resolve(): DataNode[ProjectData] = {
    workManager.compute {
      buildStackDeps()
      val packageConfigAssocs = buildPackageConfigAssocs()
      loadGhcPkgCache(packageConfigAssocs)
      buildDataNode(packageConfigAssocs)
    }
  }

  private def buildDataNode(
    packageConfigAssocs: List[PackageConfigAssoc]
  ): DataNode[ProjectData] = {
    val rootProjectName = inferRootProjectName(projectPath)
    LOG(s"rootProjectName=$rootProjectName")
    val projectDataNode = mkProjectDataNode(
      rootProjectName = rootProjectName
    )
    if (!hasRootPackageConfig(packageConfigAssocs)) {
      val projectModuleDataNode = mkDataNode(
        ProjectKeys.MODULE,
        mkModuleData(s"$rootProjectName:project")
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
    packageConfigAssocs.foreach { assoc =>
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

  private def buildStackDeps(): Unit = {
    LOG(s"Building dependencies with stack...")
    val c = new GeneralCommandLine(
      settings.stackExePath, "--stack-yaml", settings.stackYamlPath,
      "build", "--dependencies-only"
    )
    c.setWorkDirectory(settings.linkedProjectPath)
    c.setRedirectErrorStream(true)
    LOG(c.getCommandLineString)
    workManager.proc(c) { p =>
      inputStreamIterLines(p.getInputStream).foreach { line =>
        LOG(line)
      }
      val exitCode = p.waitFor()
      if (exitCode != 0) {
        throw new StackSystemException(
          s"Building stack dependencies failed with exit code $exitCode"
        )
      }
    }
  }

  private def loadGhcPkgCache(
    packageConfigAssocs: List[PackageConfigAssoc]
  ): Unit = {
    if (!HaskForceExperimentalConfigurable.isGhcPkgEnabled(settings.project)) return
    val cachedPkgs = new GhcPkgDumpExecutor(
      projectPath, settings.stackExePath, settings.stackYamlPath
    ).run()
    val cacheService = GhcPkgDumpProjectCacheService.getInstance(settings.project)
    cacheService.putPkgs(cachedPkgs)
    packageConfigAssocs.foreach { assoc =>
      assoc.packageConfig.components.foreach { component =>
        val depPkgs = component.dependencies.flatMap(cachedPkgs.firstNamed)
        component.hsSourceDirs.foreach { srcDir =>
          cacheService.addDependencyForSourcePath(
            new File(assoc.packageDir, srcDir).getCanonicalPath, depPkgs
          )
        }
      }
    }
  }

  private def buildPackageConfigAssocs(): List[PackageConfigAssoc] = {
    stackIterCabalFilePaths { it =>
      it.map { path =>
        val file = new File(path)
        val packageDir = file.getParentFile.getCanonicalPath
        val packageConfig = parsePackageConfig(file)
        PackageConfigAssoc(
          packageDir = packageDir,
          packageConfig = packageConfig
        )
      }.toList
    }
  }

  private def parsePackageConfig(file: File): PackageConfig = {
    PackageConfig.fromFile(file) match {
      case Right(Some(packageConfig)) => packageConfig
      case Right(None) =>
        throw new StackSystemException(
          "Invalid package config file",
          vars = List("file" -> file)
        )
      case Left(e) =>
        throw new StackSystemException(
          "Failed to parse package config from cabal file",
          cause = e,
          vars = List("file" -> file)
        )
    }
  }

  private def stackIterCabalFilePaths[A](f: Iterator[String] => A): A = {
    val c = new GeneralCommandLine(
      settings.stackExePath, "--stack-yaml", settings.stackYamlPath,
      "ide", "packages", "--stdout", "--cabal-files"
    )
    c.setWorkDirectory(projectPath)
    workManager.proc(c) { p =>
      val exitCode = p.waitFor()
      if (exitCode != 0) {
        val err = IOUtils.toString(p.getErrorStream, StandardCharsets.UTF_8)
        throw new StackSystemException(
          "Failed to get cabal files via 'stack'",
          vars = List(
            "exitCode" -> exitCode,
            "commandLine" -> c.getCommandLineString,
            "stderr" -> err,
          )
        )
      }
      f(inputStreamIterLines(p.getInputStream))
    }
  }

  private def inputStreamIterLines(is: InputStream): Iterator[String] = {
    val r = new BufferedReader(new InputStreamReader(is))
    Iterator.continually(r.readLine()).takeWhile(_ != null)
  }

  private def inferRootProjectName(projectPath: String): String = {
    val root = new File(projectPath)
    val rootPackageYaml = new File(root, "package.yaml")
    if (!rootPackageYaml.exists()) {
      if (!root.isDirectory) {
        throw new StackSystemException(
          "Failed to infer root project name; project path points to non-directory",
          vars = List(
            "projectPath" -> projectPath,
          )
        )
      }
      return root.getName
    }
    PsiFileParser.parseForDefaultProject[YAMLFile, File](rootPackageYaml)
      .flatMap(PackageYamlQuery.getName)
      .valueOr { e =>
        throw new StackSystemException(
          "Failed to infer root project name; failed to parse 'name' from root package.yaml",
          cause = e,
          vars = List(
            "rootPackageYaml" -> rootPackageYaml,
          )
        )
      }
  }

  private def hasRootPackageConfig(
    packageConfigAssocs: List[PackageConfigAssoc]
  ): Boolean = {
    val canonicalProjectPath = new File(projectPath).getCanonicalPath
    packageConfigAssocs.exists(assoc =>
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
