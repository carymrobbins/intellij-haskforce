package com.haskforce.highlighting.annotation.external.hsdev

import com.haskforce.settings.{HaskellBuildSettings, SettingsChangeNotifier, ToolKey, ToolSettings}
import com.haskforce.ui.tools.HaskellToolsConsole
import com.intellij.openapi.components.ProjectComponent
import com.intellij.openapi.project.Project

class HsDevProjectComponent(
  project: Project
) extends ProjectComponent with SettingsChangeNotifier {

  project.getMessageBus.connect().subscribe(
    SettingsChangeNotifier.HSDEV_TOPIC, this
  )

  private [this] var exeSettings: Option[HsDevExeSettings] =
    HsDevExeSettings.lift(
      path = Option(ToolKey.HSDEV_KEY.getPath(project)).filter(_.nonEmpty),
      flags = Option(ToolKey.HSDEV_KEY.getFlags(project)).filter(_.nonEmpty),
      stackPath = HaskellBuildSettings.getStackPathOption(project)
    )

  def getExeSettings: Option[HsDevExeSettings] = exeSettings

  private val toolsConsole =
    HaskellToolsConsole.get(project).curry(ToolKey.HSDEV_KEY)

  private val server = new HsDevServerProcess(project)

  override def onSettingsChanged(settings: ToolSettings): Unit = {
    exeSettings = HsDevExeSettings.lift(
      path = Option(settings.getPath).filter(_.nonEmpty),
      flags = Option(settings.getFlags).filter(_.nonEmpty),
      stackPath = HaskellBuildSettings.getStackPathOption(project)
    )
    toolsConsole.writeError("Settings changed, reloading hsdev")
    server.reload(exeSettings)
  }

  def currentPort: Option[Int] = server.currentPort
}

object HsDevProjectComponent {
  def get(project: Project): Option[HsDevProjectComponent] = {
    Option(project.getComponent(classOf[HsDevProjectComponent]))
  }
}
