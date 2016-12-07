package com.haskforce.jps.ghc

import java.io.File

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.execution.process.{BaseOSProcessHandler, ProcessAdapter, ProcessEvent}
import com.intellij.openapi.util.Key
import com.intellij.openapi.vfs.CharsetToolkit
import org.jetbrains.jps.incremental.CompileContext
import org.jetbrains.jps.incremental.messages.BuildMessage.Kind
import org.jetbrains.jps.incremental.messages.CompilerMessage

import scala.concurrent.ExecutionException
import scala.collection.mutable

class GhcMessageParser(moduleDir: String) {

  import GhcMessageParser._

  def process(msg: String): Seq[Result] = handle(preProcess(msg))

  /** Pre-process message string, ignoring trailing newline. */
  private def preProcess(msg: String): String = {
    msg.lastOption match {
      case Some('\n') => msg.init
      case _ => msg
    }
  }

  private def handle(msg: String): Seq[Result] = state match {
    case State.Init => processInit(msg)
    case State.PackageListWarn => processPackageListWarn(msg)
    case State.Location(kind, path, line, col) => processLocationMsg(kind, path, line, col, msg)
  }

  def flush(): Seq[Result] = {
    if (buffer.isEmpty) Nil else buildMessagesAndReset()
  }

  private def processInit(msg: String): Seq[Result] = msg match {
    case Patterns.PackageListWarnLine1() =>
      state = State.PackageListWarn
      buffer += msg
      Nil

    case Patterns.Warn(path, line, col, info) =>
      state = State.Location(Kind.WARNING, moduleDir + File.separator + path, line.toLong, col.toLong)
      buffer += info
      Nil

    case Patterns.Error(path, line, col, info) =>
      state = State.Location(Kind.ERROR, moduleDir + File.separator + path, line.toLong, col.toLong)
      buffer += info
      Nil

    case Patterns.NoLocInfo() => Nil

    case Patterns.FailingWerror() => List(Result(Kind.ERROR, msg, None, None, None))

    case _ => buildMessages(msg)
  }

  private def processPackageListWarn(msg: String): Seq[Result] = msg match {
    case Patterns.PackageListWarnLine2() => buildMessagesAndReset(msg)
    case _ => buildMessagesAndReset() ++ buildMessages(msg)
  }

  private def processLocationMsg(
    kind: Kind, path: String, line: Long, col: Long, msg: String
  ): Seq[Result] = {
    if (msg.trim.isEmpty) buildMessagesAndReset(msg) else {
      buffer += msg
      Nil
    }
  }

  private def buildMessagesAndReset(extraMessages: String*): Seq[Result] = {
    val result = buildMessages(extraMessages: _*)
    state = State.Init
    buffer.clear()
    result
  }

  private def buildMessages(extraMessages: String*): Seq[Result] = {
    val lines = (buffer ++ extraMessages).filter(_.trim.nonEmpty)
    val message = stripCommonWhitespace(lines).mkString("\n").trim
    if (message.isEmpty) Nil else {
      val result = state match {
        case State.Init => Result(Kind.INFO, message, None, None, None)
        case State.PackageListWarn => Result(Kind.WARNING, message, None, None, None)
        case State.Location(kind, path, line, col) =>
          Result(kind, message, Some(path), Some(line), Some(col))
      }
      List(result)
    }
  }

  /** Dedent lines by removing common leading whitespace. */
  private def stripCommonWhitespace(lines: Seq[String]): Seq[String] = {
    if (lines.isEmpty) lines else {
      val commonPrefixLen = lines.map(_.takeWhile(_ == ' ').length).min
      lines.map(_.drop(commonPrefixLen))
    }
  }

  private var state: State = State.Init

  private val buffer = new mutable.ArrayBuffer[String]
}

object GhcMessageParser {

  @throws[ExecutionException]
  def runProcess(
    compilerName: String,
    ctx: CompileContext,
    cmd: GeneralCommandLine,
    moduleDir: String
  ): Process = {
    val process = cmd.createProcess()
    val handler = new BaseOSProcessHandler(
      process, cmd.getCommandLineString, CharsetToolkit.getDefaultSystemCharset
    )
    val adapter = new GhcProcessAdapter(compilerName, ctx, moduleDir)
    handler.addProcessListener(adapter)
    handler.startNotify()
    handler.waitFor()
    adapter.flush()
    process
  }

  class GhcProcessAdapter(
    compilerName: String,
    context: CompileContext,
    moduleDir: String
  ) extends ProcessAdapter {

    override def onTextAvailable(event: ProcessEvent, outputType: Key[_]): Unit = {
      parser.process(event.getText).foreach { processResult }
    }

    def flush(): Unit = parser.flush().foreach { processResult }

    private def processResult(r: Result) = {
      context.processMessage(r.toCompilerMessage(compilerName))
    }

    private val parser = new GhcMessageParser(moduleDir)
  }

  final case class Result(
    kind: Kind,
    msg: String,
    path: Option[String],
    line: Option[Long],
    col: Option[Long]
  ) {
    def toCompilerMessage(compilerName: String): CompilerMessage = new CompilerMessage(
      compilerName, kind, msg, path.orNull, -1L, -1L, -1L,
      line.getOrElse(-1L), col.getOrElse(-1L)
    )
  }

  sealed trait State
  object State {
    case object Init extends State
    case object PackageListWarn extends State
    final case class Location(kind: Kind, path: String, line: Long, col: Long) extends State
  }

  object Patterns {
    val Warn = """([^:]+):(\d+):(\d+):(?: warning:| Warning:)([^\n]*)""".r
    val Error = """([^:]+):(\d+):(\d+):(?: error:)?([^\n]*)""".r
    val PackageListWarnLine1 = """Warning: The package list for '[^']+' is \d+ days old\.""".r
    val PackageListWarnLine2 = """Run '[^']+' to get the latest list of available packages\.""".r
    val NoLocInfo = """<no location info>:\s*""".r
    val FailingWerror = """Failing due to -Werror\.""".r
  }
}
