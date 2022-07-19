package com.haskforce.haskell.project.externalSystem.stack

import java.io.{BufferedReader, File, InputStream, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

import com.haskforce.settings.HaskellBuildSettings
import com.haskforce.tooling.hpack.PackageYamlQuery
import com.haskforce.utils.PsiFileParser
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import org.apache.commons.io.IOUtils
import org.jetbrains.yaml.psi.YAMLFile
import prelude._

import scala.util.control.NonFatal

class StackExecutionSettingsBuilder(
  projectPath: String,
  stackExePath: String,
  stackYamlPath: String
) {

  import StackExecutionSettingsBuilder._

  def create(): StackExecutionSettings = {
    stackRegenCabalFiles()
    val rootProjectName = inferRootProjectName(projectPath)
    val packageConfigAssocs = stackIterCabalFilePaths().map { path =>
      val file = new File(path)
      val packageDir = file.getParentFile.getCanonicalPath
      val packageConfig = parsePackageConfig(file)
      PackageConfigAssoc(
        packageDir = packageDir,
        packageConfig = packageConfig
      )
    }.toArray
    StackExecutionSettings(
      linkedProjectPath = projectPath,
      stackExePath = stackExePath,
      stackYamlPath = stackYamlPath,
      rootProjectName = rootProjectName,
      packageConfigAssocs = packageConfigAssocs
    )
  }

  private def stackRegenCabalFiles(): Unit = {
    val c = new GeneralCommandLine(
      stackExePath, "--stack-yaml", stackYamlPath,
      "build", "--dry-run"
    )
    c.setWorkDirectory(projectPath)
    val p = c.createProcess()
    val timedOut = !p.waitFor(
      STACK_REGEN_CABAL_FILE_TIMEOUT_MILLIS,
      TimeUnit.MILLISECONDS
    )
    if (timedOut) {
      LOG.warn(
        new StackSystemException(
          "Regenerating cabal files via 'stack' failed to complete within timeout",
          vars = List(
            "timeoutMillis" -> STACK_REGEN_CABAL_FILE_TIMEOUT_MILLIS,
            "commandLine" -> c.getCommandLineString,
            "workDir" -> projectPath,
          )
        )
      )
    } else if (p.exitValue() != 0) {
      val out: String = readInputStreamToStringOrFallback(p.getInputStream)
      val err: String = readInputStreamToStringOrFallback(p.getErrorStream)
      LOG.warn(
        new StackSystemException(
          "Failed to regenerate cabal files via 'stack'",
          vars = List(
            "exitCode" -> p.exitValue(),
            "commandLine" -> c.getCommandLineString,
            "workDir" -> projectPath,
            "stdout" -> s"{\n$out\n}",
            "stderr" -> s"{\n$err\n}",
          )
        )
      )
    }
  }

  private def readInputStreamToStringOrFallback(
    is: InputStream,
    fallback: String = "?"
  ): String = {
    try {
      IOUtils.toString(is, StandardCharsets.UTF_8)
    } catch {
      case NonFatal(_) => fallback
    }
  }

  private def stackIterCabalFilePaths(): Iterator[String] = {
    val c = new GeneralCommandLine(
      stackExePath, "--stack-yaml", stackYamlPath,
      "ide", "packages", "--stdout", "--cabal-files"
    )
    c.setWorkDirectory(projectPath)
    val p = c.createProcess()
    val timedOut = !p.waitFor(
      STACK_GET_CABAL_FILES_TIMEOUT_MILLIS,
      TimeUnit.MILLISECONDS
    )
    if (timedOut) {
      throw new StackSystemException(
        "Getting cabal files via 'stack' failed to complete within timeout",
        vars = List(
          "timeoutMillis" -> STACK_GET_CABAL_FILES_TIMEOUT_MILLIS,
          "commandLine" -> c.getCommandLineString,
        )
      )
    }
    if (p.exitValue() != 0) {
      val err = IOUtils.toString(p.getErrorStream, StandardCharsets.UTF_8)
      throw new StackSystemException(
        "Failed to get cabal files via 'stack'",
        vars = List(
          "exitCode" -> p.exitValue(),
          "commandLine" -> c.getCommandLineString,
          "stderr" -> err,
        )
      )
    }
    inputStreamIterLines(p.getInputStream)
  }

  private def inputStreamIterLines(is: InputStream): Iterator[String] = {
    val r = new BufferedReader(new InputStreamReader(is))
    Iterator.continually(r.readLine()).takeWhile(_ != null)
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
}

object StackExecutionSettingsBuilder {

  // 10 seconds should be plenty long enough.
  private val STACK_REGEN_CABAL_FILE_TIMEOUT_MILLIS: Long = 10000
  private val STACK_GET_CABAL_FILES_TIMEOUT_MILLIS: Long = 10000

  private val LOG = Logger.getInstance(classOf[StackExecutionSettingsBuilder])

  def forProject(
    project: Project,
    projectPath: String
  ): StackExecutionSettingsBuilder = {
    val haskellBuildSettings = HaskellBuildSettings.getInstance(project)
    val stackExePath =
      Option(haskellBuildSettings.getStackPath)
        .filter(_.nonEmpty)
        .orElse {
          //TODO
          //try {
          //  Option(ExecUtil.locateExecutableByGuessing("stack"))
          //} catch {
          //  case NonFatal(_) => None
          //}
          Some("stack")
        }.getOrElse {
          throw new StackSystemException(
            "Missing haskell build setting for stack exe path",
            vars = List("project" -> project)
          )
        }
    val stackYamlPath =
      Option(haskellBuildSettings.getStackFile)
        .filter(_.nonEmpty)
        .orElse {
          Option(new File(projectPath, "stack.yaml"))
            .filter(f => f.exists() && !f.isDirectory)
            .map(_.getCanonicalPath)
        }.getOrElse {
          throw new StackSystemException(
            "Missing Haskell build setting for stack.yaml path",
            vars = List("project" -> project)
          )
        }
    new StackExecutionSettingsBuilder(
      projectPath = projectPath,
      stackExePath = stackExePath,
      stackYamlPath = stackYamlPath
    )
  }
}
