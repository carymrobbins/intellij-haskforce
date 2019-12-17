package com.haskforce.highlighting.annotation.external.hsdev

import com.haskforce.settings.ToolKey
import com.haskforce.ui.tools.HaskellToolsConsole
import com.haskforce.utils.PortUtil
import com.intellij.execution.configurations.ParametersList
import com.intellij.execution.process.{OSProcessHandler, ProcessAdapter, ProcessEvent}
import com.intellij.ide.util.PropertiesComponent
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Key

class HsDevServerProcess private (
  project: Project,
  toolsConsole: HaskellToolsConsole.Curried
) {

  private val props = PropertiesComponent.getInstance(project)

  private [this] var state: Option[HsDevServerProcess.State] = None

  def reload(optParams: Option[HsDevExeSettings]): Unit = {
    kill()
    optParams.filter(_.enabled).foreach(spawn)
  }

  def kill(): Unit = {
    state.flatMap(_.process).foreach { p =>
      toolsConsole.writeOutput("Killing hsdev server process")
      p.getProcess.destroyForcibly()
    }
    state = None
  }

  def currentPort: Option[Int] = {
    state.map(_.port)
  }

  def isAlive: Boolean = {
    state.flatMap(_.process).exists(_.getProcess.isAlive)
  }

  private def spawn(params: HsDevExeSettings): Unit = {
    val optSuppliedPort = ToolKey.HSDEV.PORT.getValue(props)
    if (!ToolKey.HSDEV.SPAWN_SERVER.getValue(props)) {
      optSuppliedPort match {
        case Some(port) =>
          toolsConsole.writeOutput(
            s"Not spawning hsdev server, expecting a user-spawned server to exist at port $port"
          )
          state = Some(
            HsDevServerProcess.State(
              port = port,
              process = None
            )
          )

        case None =>
          toolsConsole.writeError(
            s"Configured to not spawn an hsdev server, but no port was supplied"
          )
          state = None
      }
    } else {
      val port: Int = optSuppliedPort match {
        case Some(p) =>
          toolsConsole.writeOutput(s"Spawning hsdev server process at user-configured port $p")
          p
        case None =>
          val p = PortUtil.findFreePort()
          toolsConsole.writeOutput(s"Spawning hsdev server process at free port $p")
          p
      }
      val flagParams: Array[String] = ParametersList.parse(params.flags)
      val cli = params.toGeneralCommandLine
      cli.addParameters("run", "--port", port.toString)
      cli.addParameters(flagParams: _*)
      // TODO: For stack projects, this must be the directory where the
      // stack.yaml lives. Important so that `stack exec` works as expected.
      val workDir = project.getBasePath
      cli.setWorkDirectory(workDir)
      toolsConsole.writeInput(s"${cli.getCommandLineString} ; work directory: $workDir")
      val process = new OSProcessHandler(cli)
      process.addProcessListener(new HsDevServerProcess.MyProcessListener(toolsConsole))
      state = Some(
        HsDevServerProcess.State(
          port = port,
          process = Some(process)
        )
      )
    }
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
    process: Option[OSProcessHandler]
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
