package com.haskforce.jps.stack

import java.io.File
import java.util
import java.util.concurrent.ExecutionException
import java.util.regex.Pattern

import scala.collection.JavaConversions._

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.process.{BaseOSProcessHandler, ProcessAdapter, ProcessEvent}
import com.intellij.openapi.util.Key
import com.intellij.openapi.vfs.CharsetToolkit
import org.jetbrains.jps.builders.{BuildOutputConsumer, DirtyFilesHolder}
import org.jetbrains.jps.incremental.messages.{BuildMessage, CompilerMessage}
import org.jetbrains.jps.incremental.{CompileContext, ProjectBuildException, TargetBuilder}
import org.jetbrains.jps.model.serialization.JpsModelSerializationDataService

import com.haskforce.importWizard.stack.StackYaml
import com.haskforce.jps.model.{HaskellBuildOptions, JpsHaskellBuildOptionsExtension}
import com.haskforce.jps.{HaskellSourceRootDescriptor, HaskellTarget, HaskellTargetType}

/**
 * Builds the project using `stack build`
 */
class StackBuilder extends TargetBuilder[HaskellSourceRootDescriptor, HaskellTarget](
  // NOTE: If we add HaskellTargetType.TESTS then it basically just runs the stack build
  // twice.  It's probably more practical for now to compile tests when running the
  // test suite via `stack test`
  util.Arrays.asList(HaskellTargetType.PRODUCTION)
) {
  override def getPresentableName: String = "stack"

  override def build
      (target: HaskellTarget,
       holder: DirtyFilesHolder[HaskellSourceRootDescriptor, HaskellTarget],
       outputConsumer: BuildOutputConsumer,
       context: CompileContext)
      : Unit = {
    target.getHaskellTargetType
    val jpsProject = context.getProjectDescriptor.getProject
    val opts = JpsHaskellBuildOptionsExtension.getOrCreateExtension(jpsProject).getOptions

    if (!opts.myUseStack) return

    if (opts.myStackFile.isEmpty) {
      throw new ProjectBuildException("Stack file not specified in build settings.")
    }

    if (!isStackPackage(opts.myStackFile, target)) return

    val cmd = createCommandLine(target, opts)
    runBuild(target, context, cmd)
  }

  private def isStackPackage
      (stackFile: String, target: HaskellTarget): Boolean = {
    val stackYaml = StackYaml.fromFile(stackFile).valueOr { err =>
      throw new ProjectBuildException(s"Could not parse $stackFile: $err")
    }
    val moduleBaseDir = JpsModelSerializationDataService.getBaseDirectory(target.getModule)
    val stackRoot = new File(stackFile).getParentFile
    stackYaml.packages.exists(pkg =>
      new File(stackRoot, pkg.path).getCanonicalPath == moduleBaseDir.getCanonicalPath
    )
  }

  /**
   * Creates a GeneralCommandLine which runs the following command -
   *
   *    stack build . --stack-yaml stack.yaml [flags...]
   *
   * The dot '.' after 'build' is used to specify the current package.  We set the work
   * directory to the build target module directory to ensure this works correctly.
   * By executing `stack build .` in the package directory, we ensure that only that
   * package gets built and the paths to sources displayed in compiler messages will
   * contain absolute paths.  This makes it easier later to jump to the proper source
   * locations from the compiler messages.
   */
  private def createCommandLine
      (target: HaskellTarget, opts: HaskellBuildOptions): GeneralCommandLine = {
    val baseDir = JpsModelSerializationDataService.getBaseDirectory(target.getModule)
    val cmd = new GeneralCommandLine(opts.myStackPath)
    cmd.withWorkDirectory(baseDir)
    val params = cmd.getParametersList
    params.addAll("build", ".", "--stack-yaml", opts.myStackFile)
    if (opts.myStackFlags.nonEmpty) params.addParametersString(opts.myStackFlags)
    cmd
  }

  private def runBuild
      (target: HaskellTarget, context: CompileContext, cmd: GeneralCommandLine): Unit = {
    val process = try {
      cmd.createProcess()
    } catch {
      case e: ExecutionException => throw new ProjectBuildException("Unable to start stack", e)
    }
    val handler = new BaseOSProcessHandler(
      process, cmd.getCommandLineString, CharsetToolkit.getDefaultSystemCharset
    )
    // Tell the user that we're starting to build this module.
    context.processMessage(new CompilerMessage(
      "stack", BuildMessage.Kind.INFO, s"Building '${target.getModule.getName}' ..."
    ))
    val adapter = new StackBuildProcessAdapter(context)
    handler.addProcessListener(adapter)
    handler.startNotify()
    handler.waitFor()
    if (process.exitValue() != 0) {
      throw new ProjectBuildException("Stack build failed with nonzero exit status")
    }
  }
}

class StackBuildProcessAdapter(context: CompileContext) extends ProcessAdapter {

  private[this] val state = new util.ArrayList[String]()

  override def onTextAvailable(event: ProcessEvent, outputType: Key[_]): Unit = {
    val text = event.getText
    if (text == null || text.trim.isEmpty) {
      processCompilerMessage()
      state.clear()
    } else {
      state.add(text)
    }
  }

  private def processCompilerMessage(): Unit = {
    context.processMessage(buildCompilerMessage(state.toList))
  }

  private def stripCommonWhitespace(list: List[String]) = list match {
    case Nil => Nil
    case l =>
      val commonPrefixLen = l.map(_.takeWhile(_ == ' ').length).min
      list.map(_.drop(commonPrefixLen))
  }

  private def buildCompilerMessage(message: List[String]): CompilerMessage = {
    import StackBuilderRegex._
    import BuildMessage.Kind

    val (head :: tail) = message

    def msg(k: Kind, path: String, line: String, col: String, info: String) = {
      val rebuild = if (info.trim.nonEmpty) info :: tail else tail
      val joined = stripCommonWhitespace(rebuild).mkString("\n")
      new CompilerMessage("", k, joined, path, -1L, -1L, -1L, line.toInt, col.toInt)
    }

    head.trim match {
      case WarnRegex(sourcePath, line, col, info) => msg(Kind.WARNING, sourcePath, line, col, info)
      case ErrorRegex(sourcePath, line, col, info) => msg(Kind.ERROR, sourcePath, line, col, info)
      case _ => new CompilerMessage("stack", Kind.INFO, message.mkString("\n"))
    }
  }

}

object StackBuilderRegex {
  val WarnRegex = """([^:]+):(\d+):(\d+):(?: warning:| Warning:)(.*)""".r
  val ErrorRegex = """([^:]+):(\d+):(\d+):(?: error:)?(.*)""".r
}
