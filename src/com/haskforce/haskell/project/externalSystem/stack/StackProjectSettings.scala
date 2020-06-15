package com.haskforce.haskell.project.externalSystem.stack

import com.intellij.openapi.externalSystem.settings.ExternalProjectSettings

import scala.beans.BeanProperty

final case class StackProjectSettings(
  @BeanProperty var stackYamlPath: String,
  @BeanProperty var packageConfigs: Array[PackageConfig]
) extends ExternalProjectSettings {

  override def clone(): StackProjectSettings = {
    val res = this.copy()
    copyTo(res)
    res
  }
}
