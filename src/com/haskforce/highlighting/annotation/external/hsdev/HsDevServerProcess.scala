package com.haskforce.highlighting.annotation.external.hsdev

import com.haskforce.highlighting.annotation.external.hsdev
import com.haskforce.settings.HaskellBuildSettings
import com.haskforce.ui.tools.HaskellToolsConsole
import com.haskforce.utils.PortUtil
import com.intellij.execution.configurations.{GeneralCommandLine, ParametersList}
import com.intellij.execution.process.{OSProcessHandler, ProcessAdapter, ProcessEvent, ProcessListener}
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Key

class HsDevServerProcess private (
  project: Project,
  toolsConsole: HaskellToolsConsole.Curried
) {

  private [this] var state: Option[HsDevServerProcess.State] = None

  def reload(optParams: Option[HsDevExeSettings]): Unit = {
    kill()
    optParams.foreach(spawn)
  }

  def kill(): Unit = {
    toolsConsole.writeOutput("Terminating hsdev server process")
    state.foreach(_.process.destroyProcess())
    state = None
  }

  def currentPort: Option[Int] = {
    state.filter(_.process.getProcess.isAlive).map(_.port)
  }

  private def spawn(params: HsDevExeSettings): Unit = {
    val s = HaskellBuildSettings.getInstance(project)

    val flagParams: java.util.List[String] = {
      val paramList = new ParametersList()
      paramList.addParametersString(params.flags)
      paramList.getParameters
    }

    val port = PortUtil.findFreePort()

    val (cli, paramList) =
      Option(s.getStackPath).filter(_ => s.isStackEnabled) match {
        case Some(stackPath) =>
          val cli = new GeneralCommandLine(stackPath)
          val paramList = cli.getParametersList
          paramList.addAll("exec", "--", params.path, "run")
          (cli, paramList)

        case None =>
          val cli = new GeneralCommandLine(params.path)
          val paramList = cli.getParametersList
          paramList.add("run")
          (cli, paramList)
      }

    paramList.addAll("--port", port.toString)
    paramList.addAll(flagParams)
    val process = new OSProcessHandler(cli)
    process.addProcessListener(new HsDevServerProcess.MyProcessListener(toolsConsole))
    state = Some(HsDevServerProcess.State(port, process))
  }
}

object HsDevServerProcess {

  def start(
    project: Project,
    toolsConsole: HaskellToolsConsole.Curried,
    optParams: Option[HsDevExeSettings]
  ): HsDevServerProcess = {
    val res = new HsDevServerProcess(project, toolsConsole)
    res.reload(optParams)
    res
  }

  final case class State(
    port: Int,
    process: OSProcessHandler
  )

  private class MyProcessListener(
    toolsConsole: HaskellToolsConsole.Curried
  ) extends ProcessAdapter {
    override def onTextAvailable(event: ProcessEvent, outputType: Key[_]): Unit = {
      toolsConsole.writeOutput(s"hsdev server: ${event.getText}")
    }

    override def processTerminated(event: ProcessEvent): Unit = {
      toolsConsole.writeError(s"hsdev server process terminated (exit code ${event.getExitCode})")
    }
  }
}
