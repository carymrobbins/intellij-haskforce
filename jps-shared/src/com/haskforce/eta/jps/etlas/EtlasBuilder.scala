package com.haskforce.eta.jps.etlas

import java.io.File

import com.haskforce.eta.jps.model.{EtaBuildOptions, JpsEtaBuildOptionsExtension, JpsEtlasModuleType}
import com.haskforce.jps.ghc.GhcMessageParser
import com.intellij.execution.configurations.GeneralCommandLine
import org.jetbrains.jps.ModuleChunk
import org.jetbrains.jps.builders.DirtyFilesHolder
import org.jetbrains.jps.builders.java.JavaSourceRootDescriptor
import org.jetbrains.jps.incremental.ModuleLevelBuilder.{ExitCode, OutputConsumer}
import org.jetbrains.jps.incremental._
import org.jetbrains.jps.incremental.messages.{BuildMessage, CompilerMessage}
import org.jetbrains.jps.model.module.JpsModule
import org.jetbrains.jps.model.serialization.JpsModelSerializationDataService

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionException
import scalaz.Scalaz._

/** External builder which leverages Etlas to build Eta projects. */
class EtlasBuilder extends ModuleLevelBuilder(BuilderCategory.TRANSLATOR) {

  override def getPresentableName: String = "Eta Etlas builder"

  override def build(
    context: CompileContext,
    chunk: ModuleChunk,
    dirtyFilesHolder: DirtyFilesHolder[JavaSourceRootDescriptor, ModuleBuildTarget],
    outputConsumer: OutputConsumer
  ): ExitCode = {
    val modulesIt = filterModules(chunk)
    if (modulesIt.isEmpty) return ExitCode.NOTHING_DONE
    val project = context.getProjectDescriptor.getProject
    val opts = JpsEtaBuildOptionsExtension.getOrCreate(project).getOptions
    validateOptions(opts)
    modulesIt.foreach { module =>
      val (moduleDir, cmds) = createCommandLines(module, opts)
      runBuild(module, context, cmds, moduleDir)
    }
    ExitCode.OK
  }

  private def validateOptions(opts: EtaBuildOptions): Unit = {
    List(
      "etlas" -> opts.etlasPath,
      "eta" -> opts.etaPath,
      "eta-pkg" -> opts.etaPkgPath
    ).map { case (name, path) =>
      if (path.isEmpty) {
        s"Path to '$name' is not specified in build settings".failureNel
      } else if (!new File(path).canExecute) {
        s"Path to '$name' specified in build settings is not executable: $path".failureNel
      } else {
        ().success
      }
    }.sequenceU.valueOr(errs => throw new ProjectBuildException(
      "Failed to validate build executables:\n" + errs.list.map(" * " + _).mkString("\n")
    ))
  }

  private def filterModules(chunk: ModuleChunk): Iterator[JpsModule] = {
    chunk.getModules.iterator().asScala.filter(_.getModuleType == JpsEtlasModuleType.INSTANCE)
  }

  /** Returns (moduleDir, List[(commandName, commandLine)]) */
  private def createCommandLines(
    module: JpsModule,
    opts: EtaBuildOptions
  ): (String, List[(String, GeneralCommandLine)]) = {
    // TODO: Pass javac
    val configure = new GeneralCommandLine(
      opts.etlasPath,
      "configure",
      s"--with-eta=${opts.etaPath}",
      s"--with-eta-pkg=${opts.etaPkgPath}"
    )
    val moduleDir = JpsModelSerializationDataService.getBaseDirectory(module)
    val build = new GeneralCommandLine(opts.etlasPath, "build")
    val result = List(("configure", configure), ("build", build))
    result.foreach { case (_, cmd) => cmd.setWorkDirectory(moduleDir) }
    (moduleDir.getAbsolutePath, result)
  }

  private def runBuild(
    module: JpsModule,
    context: CompileContext,
    cmds: List[(String, GeneralCommandLine)],
    moduleDir: String
  ): Unit = {
    cmds.foreach { case (cmdName, cmd) =>
      context.processMessage(new CompilerMessage(
        "etlas", BuildMessage.Kind.INFO, s"Running etlas $cmdName for '${module.getName}' ..."
      ))
      val process = try {
        GhcMessageParser.runProcess("", context, cmd, moduleDir)
      } catch {
        case e: ExecutionException => throw new ProjectBuildException(s"Unable to start etlas", e)
      }
      val exitCode = process.exitValue()
      if (exitCode != 0) {
        context.processMessage(new CompilerMessage(
          "etlas", BuildMessage.Kind.ERROR,
          s"etlas $cmdName for '${module.getName}' failed with exit status $exitCode"
        ))
        return
      }
    }
  }
}
