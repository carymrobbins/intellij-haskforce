package com.haskforce.highlighting.annotation.external.hsdev

import java.io.{BufferedInputStream, BufferedReader, ByteArrayInputStream, IOException, InputStreamReader}
import java.nio.charset.StandardCharsets
import java.util.concurrent.atomic.AtomicInteger

import com.github.plokhotnyuk.jsoniter_scala.{core => Jsoniter}
import com.haskforce.settings.ToolKey
import com.haskforce.ui.tools.HaskellToolsConsole
import com.haskforce.utils.ExecUtil
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.process.{OSProcessHandler, ProcessAdapter, ProcessEvent, ProcessOutputType}
import com.intellij.openapi.module.{Module, ModuleUtilCore}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Key
import com.intellij.psi.{PsiElement, PsiFile}
import org.apache.commons.io.IOUtils

import scala.collection.mutable
import scala.util.control.NonFatal

class HsDevExecutor private(
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
      exec[HsDevModule]("module", "--installed") match {
        case Left(()) => Vector.empty
        case Right(res) =>
          cache.installedModules = Some(res)
          res
      }
    }
  }

  def checkContents(file: PsiFile): Vector[HsDevNote[HsDevOutputMessage]] = {
    // hsdev expects a json argument for contents; e.g.
    // {"file": "path/to/file.hs", "contents": "module ..."}
    val contents = Jsoniter.writeToString(HsDevFileSource.fromPsiFile(file))
    exec[HsDevNote[HsDevOutputMessage]]("check", "--contents", contents) match {
      case Left(()) => Vector.empty
      case Right(res) => res
    }
  }

  private def mkHsdevCli(): GeneralCommandLine = {
    val cli = exeSettings.toGeneralCommandLine
    cli.addParameters("--port", port.toString)
    cli
  }

  private def renderCli(cli: GeneralCommandLine): String = {
    cli.getCommandLineString match {
      case s if s.length > 203 => s.substring(0, 200) + "..."
      case s => s
    }
  }

  private def ensureScanned(): Either[Unit, Unit] = {
    cache.scanned match {
      case HsDevCache.ScanState.NotScanned => // noop
      case HsDevCache.ScanState.Scanned =>
        if (cache.port.contains(port)) return Right(())
        // Reset the cache if the old cache was for a different server
        cache.clear()
      case HsDevCache.ScanState.ScanFailure =>
        // TODO: Hack to prevent endless looping trying to scan while failing
        return Left(())
    }
    if (cache.port.contains(port) && cache.scanned == HsDevCache.ScanState.Scanned) {
      return Right(())
    }
    val cli = mkHsdevCli()
    cli.addParameters("scan", "project", workPkgDir)
    if (exeSettings.stackPath.isDefined) {
      cli.addParameter("--stack")
    }
    val commandId = HsDevExecutor.nextCommandId()
    toolsConsole.writeInput(s"hsdev $commandId: ${renderCli(cli)}")

    // TODO: Using this sort of approach seems much better, but for some
    // reason it stalls out at the waitFor and no messages get sent via the
    // .addProcessListener. Weird.
    // val proc = new OSProcessHandler(cli)
    // proc.addProcessListener(new HsDevExecutor.MyProcessListener(toolsConsole, commandId))
    // // TODO: Add timeout config
    // val threeMinutes = 3 * 60 * 1000
    // proc.waitFor(threeMinutes)
    // if (proc.getProcess.isAlive) {
    //   toolsConsole.writeError(s"hsdev $commandId: scan took longer than $threeMinutes ms; killing")
    //   proc.destroyProcess()
    // }
    // if (proc.getExitCode != 0) {
    //   toolsConsole.writeError(
    //     s"hsdev $commandId: Failed with non-zero exit code ${proc.getExitCode}"
    //   )
    //   return Left(())
    // }

    // redirect stderr to stdout
    cli.setRedirectErrorStream(true)
    val proc = cli.createProcess()
    val stdout = new BufferedReader(new InputStreamReader(proc.getInputStream))

    Stream.continually(
      try {
        stdout.readLine()
      } catch {
        // TODO: So much sadness
        case e: IOException =>
          toolsConsole.writeError(s"hsdev $commandId: Exception when reading output: $e")
          null
      }
    ).takeWhile(_ != null).foreach(toolsConsole.writeOutput)

    // TODO: There's got to be a better way
    if (proc.isAlive) {
      toolsConsole.writeError(
        s"hsdev $commandId: process still alive after output consumed, killing"
      )
      proc.destroy()
    }

    cache.port = Some(port)
    cache.scanned = HsDevCache.ScanState.Scanned
    Right(())
  }

  private def exec[A : Jsoniter.JsonValueCodec](
    command: String,
    args: String*
  ): Either[Unit, Vector[A]] = {
    ensureScanned().flatMap { case () =>
      val cli = mkHsdevCli()
      cli.addParameter(command)
      cli.addParameters(args: _*)
      val commandId = HsDevExecutor.nextCommandId()
      toolsConsole.writeInput(s"hsdev $commandId: ${renderCli(cli)}")
      val proc = cli.createProcess()

      // TODO: Make verbose logging of JSON payloads configurable as this is very inefficient
      val stdoutBytes = IOUtils.toByteArray(proc.getInputStream)
      val stdoutString = new String(stdoutBytes, StandardCharsets.UTF_8)
      toolsConsole.writeOutput(s"hsdev $commandId: $stdoutString")
      val stdoutStream = new ByteArrayInputStream(stdoutBytes)

      HsDevData.fromJSONArrayStream[A](stdoutStream) match {
        case Left(e) =>
          toolsConsole.writeError(s"hsdev $commandId: error: $e")
          Left(())
        case Right(res) =>
          res.zipWithIndex.foreach { case (x, i) =>
            toolsConsole.writeOutput(s"hsdev $commandId: row $i:\t$x")
          }
          Right(res)
      }
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

  private class MyProcessListener(
    toolsConsole: HaskellToolsConsole.Curried,
    commandId: Int
  ) extends ProcessAdapter {
    override def onTextAvailable(event: ProcessEvent, outputType: Key[_]): Unit = {
      val text = s"hsdev $commandId: ${event.getText}"
      if (ProcessOutputType.isStdout(outputType)) {
        toolsConsole.writeOutput(text)
      } else {
        toolsConsole.writeError(text)
      }
    }
  }

  private val commandCounter = new AtomicInteger()
  private def nextCommandId(): Int = commandCounter.incrementAndGet()
}
