package com.haskforce.eta.run

import com.haskforce.HaskellIcons
import com.intellij.execution.configurations.{ConfigurationFactory, ConfigurationTypeBase, RunConfiguration}
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
