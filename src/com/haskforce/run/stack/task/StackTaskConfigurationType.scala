package com.haskforce.run.stack.task

import com.haskforce.HaskellIcons
import com.intellij.execution.configurations.{ConfigurationFactory, ConfigurationTypeBase, RunConfiguration}
import com.intellij.openapi.project.Project

class StackTaskConfigurationType extends ConfigurationTypeBase(
  "Stack Task Configuration",
  "Haskell Stack Task",
  "Execute a `stack` task.",
  HaskellIcons.FILE
) {

  addFactory(new ConfigurationFactory(this) {

    override def getId: String = "haskell-stack-task"

    override def createTemplateConfiguration(project: Project): RunConfiguration = {
      new StackTaskConfiguration(project, this)
    }
  })
}
