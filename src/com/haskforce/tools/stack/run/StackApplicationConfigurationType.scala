package com.haskforce.tools.stack.run

import com.haskforce.haskell.HaskellIcons
import com.intellij.execution.configurations.{ConfigurationFactory, ConfigurationTypeBase, RunConfiguration}
import com.intellij.openapi.project.Project

class StackApplicationConfigurationType extends ConfigurationTypeBase("Stack Run Configuration", "Haskell Stack Run", "Execute a `stack exec` task.", HaskellIcons.FILE) {

  addFactory(new ConfigurationFactory(this){
    override def createTemplateConfiguration(project: Project): RunConfiguration = {
      return new StackApplicationRunConfiguration(project, this)
    }
  })
}
