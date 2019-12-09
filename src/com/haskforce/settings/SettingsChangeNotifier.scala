package com.haskforce.settings

import com.haskforce.settings.ToolKey.{GhcModiToolSettings, HsDevToolSettings}
import com.intellij.util.messages.Topic

trait SettingsChangeNotifier[A] {
  def onSettingsChanged(settings: A)
}

object SettingsChangeNotifier {
  trait GhcModiSettingsChangeNotifier extends SettingsChangeNotifier[Option[GhcModiToolSettings]]
  val GHC_MODI_TOPIC: Topic[GhcModiSettingsChangeNotifier] =
    Topic.create("ghc-modi", classOf[GhcModiSettingsChangeNotifier])

  trait HsDevSettingsChangeNotifier extends SettingsChangeNotifier[Option[HsDevToolSettings]]
  val HSDEV_TOPIC: Topic[HsDevSettingsChangeNotifier] =
    Topic.create("hsdev", classOf[HsDevSettingsChangeNotifier])
}
