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
    state.foreach { s =>
      s.process.destroyProcess()
      // Logging after to ensure logging doesn't prevent the
      // process from getting killed.
      toolsConsole.writeOutput("Killed hsdev server process")
    }
    state = None
  }

  def currentPort: Option[Int] = {
    state.filter(_.process.getProcess.isAlive).map(_.port)
  }

  private def spawn(params: HsDevExeSettings): Unit = {
    toolsConsole.writeOutput("Spawning hsdev server process")
    val flagParams: java.util.List[String] = {
      val paramList = new ParametersList()
      paramList.addParametersString(params.flags)
      paramList.getParameters
    }
    val port = PortUtil.findFreePort()
    val cli = params.toGeneralCommandLine
    cli.addParameters("run", "--port", port.toString)
    cli.addParameters(flagParams)
    // TODO: For stack projects, this must be the directory where the
    // stack.yaml lives. Important so that `stack exec` works as expected.
    cli.setWorkDirectory(project.getBasePath)
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
