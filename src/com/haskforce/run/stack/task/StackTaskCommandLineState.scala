package com.haskforce.run.stack.task

import java.util.regex.Pattern

import com.haskforce.settings.HaskellBuildSettings
import com.haskforce.utils.FileUtil
import com.intellij.execution.configurations.{CommandLineState, GeneralCommandLine, ParametersList}
import com.intellij.execution.filters._
import com.intellij.execution.impl.ConsoleViewImpl
import com.intellij.execution.process.OSProcessHandler
import com.intellij.execution.runners.ExecutionEnvironment
import com.intellij.execution.ui.ConsoleView
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFileManager}

import scala.collection.JavaConverters._

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
        new RelativeDiscoveryFileHyperlinkRawDataFinder(getProject, Array(
          new PatternHyperlinkFormat(
            Pattern.compile("^\\s*([^:]+):(\\d+):(\\d+):"), false, false,
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

/**
  * Attempts to discover relative paths and convert them into canonical hyperlinks.
  * Normal absolute paths are unaffected by this process.
  */
class RelativeDiscoveryFileHyperlinkRawDataFinder(
  project: Project,
  linkFormats: Array[PatternHyperlinkFormat]
) extends PatternBasedFileHyperlinkRawDataFinder(linkFormats) {

  override def find(line: String): java.util.List[FileHyperlinkRawData] = {
    val res = super.find(line)
    if (res.isEmpty) return res
    val fs = LocalFileSystem.getInstance()
    res.iterator().asScala.foreach { data =>
      if (fs.findFileByPath(data.getFilePath) != null) return res
    }
    // Infer relative path, would be nice if we could figure this out somehow from the command
    // or output to determine base relative dir instead of just guessing like this.
    res.iterator().asScala.map { data =>
      val found = FileUtil.findFilesRecursively(
        project.getBaseDir,
        _.getCanonicalPath.endsWith(data.getFilePath)
      )
      // Abort if we found 0 or more than 1 match since that would be ambiguous.
      if (found.length != 1) None else {
        Some(new FileHyperlinkRawData(
          found.head.getCanonicalPath,
          data.getDocumentLine,
          data.getDocumentColumn,
          data.getHyperlinkStartInd,
          data.getHyperlinkEndInd
        ))
      }
    }.collectFirst {
      case Some(x) => java.util.Collections.singletonList(x)
    }.getOrElse(res)
  }
}
