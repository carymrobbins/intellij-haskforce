package com.haskforce.run.stack.task

import java.util.regex.Pattern

import com.haskforce.settings.HaskellBuildSettings
import com.intellij.execution.configurations.{CommandLineState, GeneralCommandLine, ParametersList}
import com.intellij.execution.filters._
import com.intellij.execution.impl.ConsoleViewImpl
import com.intellij.execution.process.OSProcessHandler
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.execution.ui.ConsoleView

class StackTaskCommandLineState(
  environment: ExecutionEnvironment,
  config: StackTaskConfiguration
) extends CommandLineState(environment) {

  setConsoleBuilder(new TextConsoleBuilderImpl(config.getProject) {
    override def getConsole: ConsoleView = {
      val consoleView = new ConsoleViewImpl(config.getProject, true)
      consoleView.addMessageFilter(new PatternBasedFileHyperlinkFilter(
        config.getProject,
        config.getProject.getBasePath,
        new PatternBasedFileHyperlinkRawDataFinder(Array(
          new PatternHyperlinkFormat(
            Pattern.compile("^([^:]+):(\\d+):(\\d+):"), false, false,
            PatternHyperlinkPart.PATH, PatternHyperlinkPart.LINE, PatternHyperlinkPart.COLUMN
          )
        ))
      ))
      consoleView
    }
  })

  override def startProcess(): OSProcessHandler = {
    val configState = config.getConfigState
    val commandLine: GeneralCommandLine = new GeneralCommandLine
    // Set up the working directory for the process
    // TODO: Make this configurable
    commandLine.setWorkDirectory(getEnvironment.getProject.getBasePath)
    val buildSettings: HaskellBuildSettings = HaskellBuildSettings.getInstance(config.getProject)
    // Set the path to `stack`
    commandLine.setExePath(buildSettings.getStackPath)
    // Build the parameters list
    val parametersList: ParametersList = commandLine.getParametersList
    parametersList.addParametersString(configState.task)
    // Set the env vars
    val environment = commandLine.getEnvironment
    if (configState.environmentVariables.isPassParentEnvs) environment.putAll(System.getenv())
    commandLine.getEnvironment.putAll(configState.environmentVariables.getEnvs)
    // Start and return the process
    new OSProcessHandler(commandLine)
  }
}
