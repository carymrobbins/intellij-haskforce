package com.haskforce.run.stack.task

import com.haskforce.settings.HaskellBuildSettings
import com.intellij.execution.configurations.{CommandLineState, GeneralCommandLine, ParametersList}
import com.intellij.execution.process.OSProcessHandler
import com.intellij.execution.runners.ExecutionEnvironment

class StackTaskCommandLineState(
  environment: ExecutionEnvironment,
  config: StackTaskConfiguration
) extends CommandLineState(environment) {
  override def startProcess(): OSProcessHandler = {
    val commandLine: GeneralCommandLine = new GeneralCommandLine
    commandLine.setWorkDirectory(getEnvironment.getProject.getBasePath)
    val buildSettings: HaskellBuildSettings = HaskellBuildSettings.getInstance(config.getProject)
    commandLine.setExePath(buildSettings.getStackPath)
    val parametersList: ParametersList = commandLine.getParametersList
    parametersList.addParametersString(config.task)
    commandLine.getEnvironment.putAll(config.getEnvs)
    new OSProcessHandler(commandLine)
  }
}
