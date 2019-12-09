package com.haskforce.highlighting.annotation.external.hsdev

import com.haskforce.settings.HaskellBuildSettings
import com.haskforce.settings.ToolKey
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.ide.util.PropertiesComponent
import com.intellij.openapi.project.Project

final case class HsDevExeSettings(
  path: String,
  flags: String,
  stackPath: Option[String]
) {
  def toGeneralCommandLine: GeneralCommandLine = {
    stackPath match {
      case Some(stack) =>
        val cli = new GeneralCommandLine(stack)
        cli.addParameters("exec", "--", path)
        cli.getParametersList.addParametersString(flags)
        cli

      case None =>
        val cli = new GeneralCommandLine(path)
        cli.getParametersList.addParametersString(flags)
        cli
    }
  }
}

object HsDevExeSettings {

  def get(project: Project): Option[HsDevExeSettings] = {
    of(
      ToolKey.HSDEV.getValue(PropertiesComponent.getInstance(project)),
      HaskellBuildSettings.getStackPathOption(project)
    )
  }

  def of(
    toolSettings: ToolKey.HsDevToolSettings,
    stackPath: Option[String]
  ): Option[HsDevExeSettings] = {
    toolSettings.path.map { path =>
      HsDevExeSettings(
        path = path,
        flags = toolSettings.flags,
        stackPath = stackPath
      )
    }
  }
}
