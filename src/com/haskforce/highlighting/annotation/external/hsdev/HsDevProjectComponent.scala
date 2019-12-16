package com.haskforce.highlighting.annotation.external.hsdev

import com.haskforce.settings.SettingsChangeNotifier.HsDevSettingsChangeNotifier
import com.haskforce.settings.{HaskellBuildSettings, SettingsChangeNotifier, ToolKey}
import com.haskforce.ui.tools.HaskellToolsConsole
import com.intellij.openapi.components.ProjectComponent
import com.intellij.openapi.project.Project

class HsDevProjectComponent(
  project: Project
) extends ProjectComponent with HsDevSettingsChangeNotifier {

  project.getMessageBus.connect().subscribe(
    SettingsChangeNotifier.HSDEV_TOPIC, this
  )

  def getExeSettings: Option[HsDevExeSettings] = exeSettings

  def isConfigured: Boolean = exeSettings.isDefined

  private [this] var exeSettings: Option[HsDevExeSettings] =
    HsDevExeSettings.get(project)

  private val toolsConsole =
    HaskellToolsConsole.get(project).curry(ToolKey.HSDEV)

  private val server = HsDevServerProcess.start(
    project, toolsConsole, exeSettings
  )

  override def onSettingsChanged(settings: ToolKey.HsDevToolSettings): Unit = {
    exeSettings = HsDevExeSettings.of(
      settings,
      HaskellBuildSettings.getStackPathOption(project)
    )
    toolsConsole.writeError("Settings changed, reloading hsdev")
    server.reload(exeSettings)
  }

  override def disposeComponent(): Unit = {
    kill()
  }

  def currentPort: Option[Int] = server.currentPort

  def restart(): Unit = {
    exeSettings = HsDevExeSettings.get(project)
    server.reload(exeSettings)
  }

  def kill(): Unit = {
    server.kill()
  }
}

object HsDevProjectComponent {

  def get(project: Project): Option[HsDevProjectComponent] = {
    Option(project.getComponent(classOf[HsDevProjectComponent]))
  }
}
