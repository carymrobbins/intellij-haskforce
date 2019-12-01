package com.haskforce.highlighting.annotation.external.hsdev

import com.haskforce.settings.HaskellBuildSettings
import com.haskforce.utils.PortUtil
import com.intellij.execution.configurations.{GeneralCommandLine, ParametersList}
import com.intellij.execution.process.OSProcessHandler
import com.intellij.openapi.project.Project

class HsDevServerProcess(project: Project) {

  private [this] var state: Option[HsDevServerProcess.State] = None

  def reload(optParams: Option[HsDevExeSettings]): Unit = {
    state.foreach(_.process.destroyProcess())
    optParams.foreach(spawn)
  }

  def currentPort: Option[Int] = state.map(_.port)

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
    state = Some(HsDevServerProcess.State(port, process))
  }
}

object HsDevServerProcess {
  final case class State(
    port: Int,
    process: OSProcessHandler
  )
}
