package com.haskforce.highlighting.annotation.external.hsdev

import com.haskforce.settings.HaskellBuildSettings
import com.haskforce.settings.ToolKey
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.ide.util.PropertiesComponent
import com.intellij.openapi.project.Project

final case class HsDevExeSettings(
  toolSettings: ToolKey.HsDevToolSettings,
  stackPath: Option[String]
) {
  def toGeneralCommandLine: GeneralCommandLine = {
    stackPath match {
      case Some(stack) =>
        val cli = new GeneralCommandLine(stack)
        cli.addParameters("exec", "--", toolSettings.path)
        cli.getParametersList.addParametersString(toolSettings.flags)
        cli

      case None =>
        val cli = new GeneralCommandLine(toolSettings.path)
        cli.getParametersList.addParametersString(toolSettings.flags)
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
    optSettings: Option[ToolKey.HsDevToolSettings],
    stackPath: Option[String]
  ): Option[HsDevExeSettings] = {
    optSettings.map(HsDevExeSettings(_, stackPath))
  }
}
