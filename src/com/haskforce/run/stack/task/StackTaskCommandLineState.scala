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
