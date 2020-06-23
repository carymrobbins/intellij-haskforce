package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.settings.ExternalProjectSettings

final case class StackProjectSettings(
  var executionSettings: StackExecutionSettings
) extends ExternalProjectSettings {

  override def clone(): StackProjectSettings = {
    val res = this.copy()
    copyTo(res)
    res
  }

  override def getExternalProjectPath: String = {
    executionSettings.linkedProjectPath
  }

  override def setExternalProjectPath(externalProjectPath: String): Unit = {
    this.executionSettings = this.executionSettings.copy(
      linkedProjectPath = externalProjectPath
    )
  }
}
