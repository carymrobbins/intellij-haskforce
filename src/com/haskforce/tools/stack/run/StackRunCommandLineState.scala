package com.haskforce.haskell.run.stack

import com.haskforce.system.settings.HaskellBuildSettings
import com.intellij.execution.configurations.{ParametersList, GeneralCommandLine, CommandLineState}
import com.intellij.execution.process.{OSProcessHandler}
import com.intellij.execution.runners.ExecutionEnvironment

class StackRunCommandLineState(environment: ExecutionEnvironment,
                               config: StackApplicationRunConfiguration) extends CommandLineState(environment) {

  // run `stack exec <executable> -- <executableArguments>`
  override def startProcess(): OSProcessHandler = {
    val commandLine: GeneralCommandLine = new GeneralCommandLine
    commandLine.setWorkDirectory(getEnvironment.getProject.getBasePath)
    val buildSettings: HaskellBuildSettings = HaskellBuildSettings.getInstance(config.getProject)
    commandLine.setExePath(buildSettings.getStackPath)
    val parametersList: ParametersList = commandLine.getParametersList
    parametersList.add("exec")
    parametersList.add(config.executable)
    parametersList.add("--")
    parametersList.addParametersString(config.executableArguments)
    return new OSProcessHandler(commandLine)
  }
}
