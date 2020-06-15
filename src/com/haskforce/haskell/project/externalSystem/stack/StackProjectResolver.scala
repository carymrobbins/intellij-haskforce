package com.haskforce.haskell.project.externalSystem.stack

import java.io.{BufferedReader, File, InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

import com.haskforce.HaskellModuleType
import com.haskforce.cabal.lang.psi.CabalFile
import com.haskforce.tooling.hpack.PackageYamlQuery
import com.haskforce.utils.{PsiFileParser, YAMLFileUtil}
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.externalSystem.model.project.{ContentRootData, ExternalSystemSourceType, ModuleData, ProjectData}
import com.intellij.openapi.externalSystem.model.task.{ExternalSystemTaskId, ExternalSystemTaskNotificationListener}
import com.intellij.openapi.externalSystem.model.{DataNode, ExternalSystemException, Key, ProjectKeys}
import com.intellij.openapi.externalSystem.service.project.ExternalSystemProjectResolver
import org.apache.commons.io.IOUtils
import prelude._

final class StackProjectResolver extends ExternalSystemProjectResolver[StackExecutionSettings] {

  override def resolveProjectInfo(
    id: ExternalSystemTaskId,
    projectPath: String,
    isPreviewMode: Boolean,
    settings: StackExecutionSettings,
    listener: ExternalSystemTaskNotificationListener
  ): DataNode[ProjectData] = {
    stackRegenCabalFiles(projectPath, settings)
    val rootProjectName = inferRootProjectName(projectPath)
    val projectDataNode = mkProjectDataNode(
      rootProjectName = rootProjectName,
      projectPath = projectPath
    )
    stackIterCabalFilePaths(projectPath, settings).foreach { path =>
      val file = new File(path)
      val packageDir = file.getParentFile.getCanonicalPath
      val cabalFile = parseCabalFile(file)
      val packageConfig = parsePackageConfig(path, cabalFile)
      projectDataNode.addChild(
        mkPackageModuleDataNode(
          projectPath = projectPath,
          packageDir = packageDir,
          packageConfig = packageConfig,
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
        throw new ExternalSystemException(
          "Too many 'library' components for package"
            + s"; libraryCount=${libs.length}"
            + s"; packageConfig='${packageConfig.name}'"
            + s"; packageDir='$packageDir'"
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

  private def stackRegenCabalFiles(
    projectPath: String,
    settings: StackExecutionSettings
  ): Unit = {
    val c = new GeneralCommandLine(
      settings.stackExePath, "--stack-yaml", settings.stackYamlPath,
      "build", "--dry-run"
    )
    c.setWorkDirectory(projectPath)
    val p = c.createProcess()
    val timedOut = !p.waitFor(
      StackProjectResolver.STACK_REGEN_CABAL_FILE_TIMEOUT_MILLIS,
      TimeUnit.MILLISECONDS
    )
    if (timedOut) {
      throw new ExternalSystemException(
        "Regenerating cabal files via 'stack' failed to complete within timeout"
          + s"; timeoutMillis=${StackProjectResolver.STACK_REGEN_CABAL_FILE_TIMEOUT_MILLIS}"
          + s"; commandLine='${c.getCommandLineString}'"
      )
    }
    if (p.exitValue() != 0) {
      val err = IOUtils.toString(p.getErrorStream, StandardCharsets.UTF_8)
      throw new ExternalSystemException(
        "Failed to regenerate cabal files via 'stack'"
          + s"; exitCode=${p.exitValue()}"
          + s"; commandLine='${c.getCommandLineString}'"
          + s"; stderr=$err"
      )
    }
  }

  private def stackIterCabalFilePaths(
    projectPath: String,
    settings: StackExecutionSettings
  ): Iterator[String] = {
    val c = new GeneralCommandLine(
      settings.stackExePath, "--stack-yaml", settings.stackYamlPath,
      "ide", "packages", "--stdout", "--cabal-files"
    )
    c.setWorkDirectory(projectPath)
    val p = c.createProcess()
    val timedOut = !p.waitFor(
      StackProjectResolver.STACK_GET_CABAL_FILES_TIMEOUT_MILLIS,
      TimeUnit.MILLISECONDS
    )
    if (timedOut) {
      throw new ExternalSystemException(
        "Getting cabal files via 'stack' failed to complete within timeout"
          + s"; timeoutMillis=${StackProjectResolver.STACK_GET_CABAL_FILES_TIMEOUT_MILLIS}"
          + s"; commandLine='${c.getCommandLineString}'"
      )
    }
    if (p.exitValue() != 0) {
      val err = IOUtils.toString(p.getErrorStream, StandardCharsets.UTF_8)
      throw new ExternalSystemException(
        "Failed to get cabal files via 'stack'"
          + s"; exitCode=${p.exitValue()}"
          + s"; commandLine='${c.getCommandLineString}'"
          + s"; stderr=$err"
      )
    }
    inputStreamIterLines(p.getInputStream)
  }

  private def inputStreamIterLines(is: InputStream): Iterator[String] = {
    val r = new BufferedReader(new InputStreamReader(is))
    Iterator.continually(r.readLine()).takeWhile(_ != null)
  }

  private def parseCabalFile(file: File): CabalFile = {
    PsiFileParser.parseForDefaultProject[CabalFile, File](file) match {
      case Right(cabalFile) => cabalFile
      case Left(e) =>
        throw new ExternalSystemException(
          s"Failed to parse cabal file: ${file.getPath}",
          e
        )
    }
  }

  private def parsePackageConfig(path: String, cabalFile: CabalFile): PackageConfig = {
    PackageConfig.fromCabalFile(cabalFile) match {
      case Right(packageConfig) => packageConfig
      case Left(e) =>
        throw new ExternalSystemException(
          s"Failed to parse package config from cabal file: $path",
          e
        )
    }
  }

  @throws[ExternalSystemException]
  private def inferRootProjectName(projectPath: String): String = {
    val root = new File(projectPath)
    val rootPackageYaml = new File(root, "package.yaml")
    if (!rootPackageYaml.exists()) {
      if (!root.isDirectory) {
        throw new ExternalSystemException(
          "Failed to infer root project name"
            + "; project path points to non-directory"
            + s": '$projectPath'"
        )
      }
      return root.getName
    }
    YAMLFileUtil.parseYaml(rootPackageYaml)
      .flatMap(PackageYamlQuery.getName)
      .valueOr { e =>
        throw new ExternalSystemException(
          s"Failed to infer root project name"
            + "; failed to parse 'name' from root package.yaml"
            + s": '$rootPackageYaml'",
          e
        )
      }
  }
}

object StackProjectResolver {
  //private val LOG = Logger.getInstance(classOf[StackProjectResolver])

  // 5 seconds should be plenty long enough.
  private val STACK_REGEN_CABAL_FILE_TIMEOUT_MILLIS = 5000
  private val STACK_GET_CABAL_FILES_TIMEOUT_MILLIS = 5000
}
