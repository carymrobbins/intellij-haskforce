package com.haskforce.utils

import com.haskforce.highlighting.annotation.external.GhcModUtil
import com.haskforce.settings.HaskellBuildSettings
import com.intellij.execution.ExecutionException
import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.process.CapturingProcessHandler
import java.io.File

import com.intellij.openapi.project.Project

/**
 * Helper class to execute cabal based on compiler settings.
 */
object CabalExecutor {
  def create(project: Project, workDir: Option[String]): Either[CabalExecutorError, CabalExecutor] = {
    create(HaskellBuildSettings.getInstance(project).getCabalPath, workDir)
  }

  def create(cabalPath: String, workDir: Option[String]): Either[CabalExecutorError, CabalExecutor] = {
    if (cabalPath.isEmpty) return Left(NotConfigured)
    if (!new File(cabalPath).canExecute) return Left(NotExecutable)
    Right(create { () =>
      val commandLine = new GeneralCommandLine(cabalPath)
      workDir.foreach(commandLine.setWorkDirectory)
      commandLine
    })
  }

  def create(factory: () => GeneralCommandLine): CabalExecutor = CabalExecutor(factory)

  sealed trait CabalExecutorError
  object NotConfigured extends CabalExecutorError
  object NotExecutable extends CabalExecutorError
}

case class CabalExecutor private (factory: () => GeneralCommandLine) {
  def getNumericVersion: String = rawExec("--numeric-version")

  def rawCommandLine(): GeneralCommandLine = factory()

  def rawCommandLine(args: String*): GeneralCommandLine = {
    val commandLine = rawCommandLine()
    commandLine.addParameters(args: _*)
    commandLine
  }

  def rawExec(args: String*): String = ExecUtil.readCommandLine(rawCommandLine(args: _*))

  @throws(classOf[ExecutionException])
  def init(project: Project, args: Seq[String]): String = {
    val commandLine = initPreEnv(args)
    // TODO: We need to patch the PATH since `cabal init` doesn't support --with-ghc
    GhcModUtil.updateEnvironment(project, commandLine.getEnvironment)
    initPostEnv(commandLine)
  }

  @throws(classOf[ExecutionException])
  def init(ghcPath: String, args: Seq[String]): String = {
    val commandLine = initPreEnv(args)
    // TODO: We need to patch the PATH since `cabal init` doesn't support --with-ghc
    GhcModUtil.updateEnvironment(commandLine.getEnvironment, ghcPath)
    initPostEnv(commandLine)
  }

  private def initPreEnv(args: Seq[String]) = rawCommandLine("init" +: args: _*)

  private def initPostEnv(commandLine: GeneralCommandLine): String = {
    val process = commandLine.createProcess()
    val output = new CapturingProcessHandler(process).runProcess
    if (output.getExitCode == 0) return output.getStdout
    throw new ExecutionException(output.getStderr)
  }
}
