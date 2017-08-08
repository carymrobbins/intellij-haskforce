package com.haskforce.eta.run

import com.haskforce.HaskellIcons
import com.intellij.execution.configurations.{ConfigurationFactory, ConfigurationType, ConfigurationTypeBase, RunConfiguration}
import com.intellij.openapi.extensions.Extensions
import com.intellij.openapi.project.Project

class EtlasConfigurationType extends ConfigurationTypeBase(
  "Etlas Task Configuration",
  "Etlas Task",
  "Execute an Etlas task",
  HaskellIcons.ETA_FILE
) {
  addFactory(new ConfigurationFactory(this) {
    override def createTemplateConfiguration(project: Project): RunConfiguration = {
      new EtlasRunConfiguration(project, this)
    }
  })
}

object EtlasConfigurationType {
  val INSTANCE = Extensions.findExtension(
    ConfigurationType.CONFIGURATION_TYPE_EP,
    classOf[EtlasConfigurationType]
  )
}
