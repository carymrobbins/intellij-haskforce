package com.haskforce.highlighting.annotation.external.hsdev

import com.intellij.execution.configurations.GeneralCommandLine

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

  def lift(
    path: Option[String],
    flags: Option[String],
    stackPath: Option[String]
  ): Option[HsDevExeSettings] = {
    path.map(_.trim).filter(_.nonEmpty).map(thePath =>
      HsDevExeSettings(
        path = thePath,
        flags = flags.map(_.trim).getOrElse(""),
        stackPath = stackPath
      )
    )
  }
}
