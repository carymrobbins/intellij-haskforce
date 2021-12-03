package com.haskforce.haskell.project.externalSystem.stack

import java.util

import com.intellij.openapi.Disposable
import com.intellij.openapi.components.{PersistentStateComponent, State, Storage}
import com.intellij.openapi.externalSystem.settings.{AbstractExternalSystemSettings, ExternalSystemSettingsListener}
import com.intellij.openapi.project.Project
import com.intellij.util.xmlb.annotations.XCollection

@State(name = "StackSettings", storages = Array(new Storage("haskell-stack.xml")))
final class StackSettings(
  project: Project
) extends AbstractExternalSystemSettings[
    StackSettings,
    StackProjectSettings,
    StackProjectSettingsListener
  ](StackTopic, project)
  with PersistentStateComponent[StackSettings.State] {

  override def getState: StackSettings.State = {
    val state = new StackSettings.State
    fillState(state)
    state
  }

  override def loadState(state: StackSettings.State): Unit = {
    super[AbstractExternalSystemSettings].loadState(state)
  }

  override def subscribe(
    listener: ExternalSystemSettingsListener[StackProjectSettings]
  ): Unit = {
    doSubscribe(
      new StackDelegatingExternalSystemSettingsListener(listener),
      project
    )
  }

  override def subscribe(
    listener: ExternalSystemSettingsListener[StackProjectSettings],
    parentDisposable: Disposable
  ): Unit = {
    doSubscribe(
      new StackDelegatingExternalSystemSettingsListener(listener),
      parentDisposable
    )
  }

  override def copyExtraSettingsFrom(settings: StackSettings): Unit = {}

  override def checkSettings(
    old: StackProjectSettings,
    current: StackProjectSettings
  ): Unit = {
    if (old != current) getPublisher.onStackProjectSettingsChange()
  }
}

object StackSettings {

  def getInstance(project: Project): StackSettings = {
    project.getService(classOf[StackSettings])
  }

  class State extends AbstractExternalSystemSettings.State[StackProjectSettings] {

    private val linkedProjectSettings: util.TreeSet[StackProjectSettings] = {
      new util.TreeSet[StackProjectSettings]()
    }

    @XCollection(elementTypes = Array(classOf[StackProjectSettings]))
    override def getLinkedExternalProjectsSettings: util.Set[StackProjectSettings] = {
      linkedProjectSettings
    }

    override def setLinkedExternalProjectsSettings(settings: util.Set[StackProjectSettings]): Unit = {
      if (settings == null) return
      linkedProjectSettings.addAll(settings)
      ()
    }
  }
}
