package com.haskforce.haskell.project.externalSystem.stack

import java.io.{BufferedReader, File, InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.util

import com.haskforce.HaskellModuleType
import com.haskforce.tooling.hpack.PackageYamlQuery
import com.haskforce.tooling.stack.PackageConfigCacheService
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

  import StackProjectInfoResolver._

  private def LOG(text: String, stdOut: Boolean = true): Unit = {
    listener.onTaskOutput(id, text + "\n", stdOut)
  }

  // Runs in 'workManager.compute' to ensure that the thread is cancellable.
  // Any subprocesses should be spawned via the 'workManager' to ensure
  // they are cancellable.
  def resolve(): DataNode[ProjectData] = {
    workManager.compute {
      syncCabalFiles()
      val dependencyVersionResolver = getDependencyVersionResolver()
      val packageConfigAssocs = buildPackageConfigAssocs(dependencyVersionResolver)
      PackageConfigCacheService
        .getInstance(settings.project)
        .setPackageConfigAssocs(packageConfigAssocs)
      buildDataNode(packageConfigAssocs)
    }
  }

  private def syncCabalFiles(): Unit = {
    LOG("Synchronizing cabal files...")
    val c = new GeneralCommandLine(
      settings.stackExePath, "--stack-yaml", settings.stackYamlPath,
      "build", "--dependencies-only", "--dry-run"
    )
    c.setWorkDirectory(settings.linkedProjectPath)
    c.setRedirectErrorStream(true)
    workManager.proc(c) { p =>
      inputStreamIterLines(p.getInputStream).foreach { line =>
        LOG(line)
      }
      val exitCode = p.waitFor()
      if (exitCode != 0) {
        throw new StackSystemException(
          "Failed to synchronize cabal files",
          vars = List(
            "exitCode" -> exitCode,
            "command" -> c.getCommandLineString
          )
        )
      }
    }
  }

  private def getDependencyVersionResolver(): DependencyVersionResolver = {
    LOG("Getting dependency versions...")
    val c = new GeneralCommandLine(
      settings.stackExePath, "--stack-yaml", settings.stackYamlPath,
      "ls", "dependencies", "--separator", ":"
    )
    c.setWorkDirectory(settings.linkedProjectPath)
    workManager.proc(c) { p =>
      val dependencyVersionResolver = new DependencyVersionResolver
      inputStreamIterLines(p.getInputStream).foreach { line =>
        line.split(":", 2) match {
          case Array(dep, version) =>
            dependencyVersionResolver.addDependencyVersion(dep, version)
          case _ =>
            throw new StackSystemException(
              "Invalid dependency line encountered",
              vars = List(
                "command" -> c.getCommandLineString,
                "line" -> line,
              )
            )
        }
      }
      val exitCode = p.waitFor()
      if (exitCode != 0) {
        inputStreamIterLines(p.getErrorStream).foreach { line =>
          LOG(line, stdOut = false)
        }
        throw new StackSystemException(
          "Failed to get dependency versions",
          vars = List(
            "exitCode" -> exitCode,
            "command" -> c.getCommandLineString,
          )
        )
      }
      dependencyVersionResolver
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

  private def buildPackageConfigAssocs(
    dependencyVersionResolver: DependencyVersionResolver
  ): List[PackageConfigAssoc] = {
    stackIterCabalFilePaths { it =>
      it.map { path =>
        val file = new File(path)
        val packageDir = file.getParentFile.getCanonicalPath
        val packageConfig = parsePackageConfig(file)
        PackageConfigAssoc(
          packageDir = packageDir,
          packageConfig = withDependencyVersions(
            packageConfig,
            dependencyVersionResolver
          )
        )
      }.toList
    }
  }

  private def withDependencyVersions(
    packageConfig: PackageConfig,
    dependencyVersionResolver: DependencyVersionResolver
  ): PackageConfig = {
    packageConfig.copy(
      components = packageConfig.components.map(c =>
        c.copy(
          dependencies = c.dependencies.map(d =>
            dependencyVersionResolver.resolveDependencyVersion(d)
          )
        )
      )
    )
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

    val deps = new java.util.HashMap[String, (PackageConfig.Dependency, DependencyScope)]
    packageConfig.components.foreach { component =>
      getDependencyType(component) match {
        case DependencyType.Compile =>
          // This may, intentionally, overwrite any TEST deps as COMPILE deps.
          component.dependencies.foreach { dep =>
            deps.put(dep.name, (dep, DependencyScope.COMPILE))
          }
        case DependencyType.Test =>
          // We don't want duplicate deps. Any already included in COMPILE
          // don't need to be re-included in TEST.
          component.dependencies.foreach { dep =>
            deps.putIfAbsent(dep.name, (dep, DependencyScope.TEST))
          }
      }
    }
    deps.forEach { case (_, (dep, scope)) =>
      packageModuleDataNode.addChild(
        mkLibDepNode(packageModuleData, dep, scope)
      )
    }

    packageModuleDataNode
  }

  private def mkLibDepNode(
    ownerModule: ModuleData,
    dep: PackageConfig.Dependency,
    dependencyScope: DependencyScope
  ): DataNode[LibraryDependencyData] = {
    val libData = new LibraryData(
      StackManager.PROJECT_SYSTEM_ID,
      dep.name
    )
    dep.version.foreach { v =>
      libData.setExternalName(s"${dep.name}:$v")
      libData.setVersion(v)
    }
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
}

object StackProjectInfoResolver {

  private[StackProjectInfoResolver] class DependencyVersionResolver {

    /**
     * If the supplied 'dep' already has its 'version' field set, simply return it.
     * Otherwise, attempt to set its 'version' from our internal mapping.
     */
    def resolveDependencyVersion(dep: PackageConfig.Dependency): PackageConfig.Dependency = {
      if (dep.version.isDefined) return dep
      dep.copy(version = Option(internal.get(dep.name)))
    }

    /** Adds a configured dependency to the internal mapping. */
    def addDependencyVersion(dep: String, version: String): Unit = {
      val existing = internal.put(dep, version)
      if (existing != null) {
        throw new IllegalStateException(
          s"Dependency specified multiple times: $dep; versions: $existing, $version"
        )
      }
    }

    private val internal = new util.HashMap[String, String]
  }

  private[StackProjectInfoResolver] sealed trait DependencyType
  private[StackProjectInfoResolver] object DependencyType {
    case object Compile extends DependencyType
    case object Test extends DependencyType
  }
}
