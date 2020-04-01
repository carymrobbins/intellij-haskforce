package com.haskforce.eta.run

import com.intellij.execution.configurations.{ConfigurationFactory, RunConfiguration}
import com.intellij.openapi.project.Project

object EtlasConfigurationFactory extends ConfigurationFactory(EtlasConfigurationType.INSTANCE) {
  override def createTemplateConfiguration(project: Project): RunConfiguration = {
    new EtlasRunConfiguration(project, this)
  }
}
