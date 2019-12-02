package com.haskforce.highlighting.annotation.external.hsdev

import java.io.{BufferedReader, IOException, InputStreamReader}

import com.github.plokhotnyuk.jsoniter_scala.core.{JsonValueCodec, scanJsonArrayFromStream}
import com.haskforce.settings.ToolKey
import com.haskforce.ui.tools.HaskellToolsConsole
import com.haskforce.utils.ExecUtil
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiElement

import scala.collection.mutable
import scala.util.control.NonFatal

class HsDevExecutor(
  project: Project,
  module: Module,
  workPkgDir: String,
  exeSettings: HsDevExeSettings,
  port: Int,
  cache: HsDevCache,
  toolsConsole: HaskellToolsConsole.Curried
) {
  def installedModules: Vector[HsDevModule] = {
    cache.installedModules.getOrElse {
      exec[HsDevModule]("module", "--installed").fold(
        (_: Unit) => Vector.empty,
        res => {
          cache.installedModules = Some(res)
          res
        }
      )
    }
  }

  private def mkHsdevCli(): GeneralCommandLine = {
    val cli = exeSettings.toGeneralCommandLine
    cli.addParameters("--port", port.toString)
    cli
  }

  private def ensureScanned(): Either[Unit, Unit] = {
    val cli = mkHsdevCli()
    cli.addParameters("scan", "project", workPkgDir)
    if (exeSettings.stackPath.isDefined) {
      cli.addParameter("--stack")
    }
    toolsConsole.writeInput(cli.getCommandLineString)
    val proc = cli.createProcess()
    val stdout = new BufferedReader(new InputStreamReader(proc.getInputStream))
    val stderr = new BufferedReader(new InputStreamReader(proc.getErrorStream))
    while (proc.isAlive) {
      try {
        Option(stdout.readLine()).foreach(toolsConsole.writeOutput)
      } catch {
        case e: IOException =>
          toolsConsole.writeError(
            s"Failed to read stdout of command '${cli.getCommandLineString}': $e"
          )
          return Left(())
      }
      try {
        Option(stderr.readLine()).foreach(toolsConsole.writeError)
      } catch {
        case e: IOException =>
          toolsConsole.writeError(
            s"Failed to read stderr of command '${cli.getCommandLineString}': $e"
          )
          return Left(())
      }
    }
    if (proc.exitValue != 0) {
      toolsConsole.writeError(
        s"Command '${cli.getCommandLineString}' with non-zero exit code ${proc.exitValue}"
      )
      return Left(())
    }
    Right(())
  }

  private def exec[A : JsonValueCodec](command: String, args: String*): Either[Unit, Vector[A]] = {
    ensureScanned() match {
      case Left(()) => return Left(())
      case Right(()) => // noop
    }
    val cli = mkHsdevCli()
    cli.addParameter(command)
    cli.addParameters(args: _*)
    toolsConsole.writeInput(cli.getCommandLineString)
    val proc = cli.createProcess()
    try {
      val stdout = proc.getInputStream
      val buf = new mutable.ArrayBuffer[A]
      scanJsonArrayFromStream(stdout) { a: A => buf += a ; true }
      Right(buf.toVector)
    } catch {
      case NonFatal(e) =>
        val stderrText =
          try {
            val stderr = new BufferedReader(new InputStreamReader(proc.getErrorStream))
            Stream.continually(stderr.readLine())
              .takeWhile(_ != null)
              .mkString("\n")
          } catch {
            case _: IOException => ""
          }
        toolsConsole.writeError(
          s"Command '${cli.getCommandLineString}' failed\n"
            + s"  stderr was: $stderrText"
            + s"  exception was: $e"
        )
        Left(())
    }
  }
}

object HsDevExecutor {
  def get(element: PsiElement): Option[HsDevExecutor] = {
    val project = element.getProject
    for {
      module <- Option(ModuleUtilCore.findModuleForPsiElement(element))
      workPkgDir = ExecUtil.guessWorkDir(module)
      projectComponent <- HsDevProjectComponent.get(project)
      exeSettings <- projectComponent.getExeSettings
      port <- projectComponent.currentPort
      moduleComponent <- HsDevModuleComponent.get(module)
      cache = moduleComponent.cache
      toolsConsole = HaskellToolsConsole.get(project).curry(ToolKey.HSDEV_KEY)
    } yield new HsDevExecutor(
      project = project,
      module = module,
      workPkgDir = workPkgDir,
      exeSettings = exeSettings,
      port = port,
      cache = cache,
      toolsConsole = toolsConsole
    )
  }
}
