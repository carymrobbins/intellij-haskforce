package com.haskforce.haskell.project.externalSystem.stack

import java.util

import com.intellij.openapi.externalSystem.ExternalSystemManager
import com.intellij.openapi.externalSystem.settings.{ExternalProjectSettings, ExternalSystemSettingsListenerEx}
import com.intellij.openapi.project.Project

// TODO: Do we need this?
class StackProjectSettingsUpdater extends ExternalSystemSettingsListenerEx {
  override def onProjectsLinked(
    project: Project,
    manager: ExternalSystemManager[_, _, _, _, _],
    settings: util.Collection[_ <: ExternalProjectSettings]
  ): Unit = {
    throw new NotImplementedError("StackProjectSettingsUpdater.onProjectsLinked")
  }

  override def onProjectsLoaded(
    project: Project,
    manager: ExternalSystemManager[_, _, _, _, _],
    settings: util.Collection[_ <: ExternalProjectSettings]
  ): Unit = {
    throw new NotImplementedError("StackProjectSettingsUpdater.onProjectsLoaded")
  }

  override def onProjectsUnlinked(
    project: Project,
    manager: ExternalSystemManager[_, _, _, _, _],
    linkedProjectPaths: util.Set[String]
  ): Unit = {
    throw new NotImplementedError("StackProjectSettingsUpdater.onProjectsUnlinked")
  }
}
