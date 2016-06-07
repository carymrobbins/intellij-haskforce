package com.haskforce.tools

import java.io._
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

import scala.collection.JavaConverters._
import scala.concurrent.ExecutionException
import scalaz.\/

import com.intellij.execution.configurations.GeneralCommandLine
import com.intellij.openapi.components.ProjectComponent
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.module.ModuleUtilCore
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiFile, PsiManager}

import com.haskforce.HaskellModuleType
import com.haskforce.Implicits._
import com.haskforce.cabal.lang.psi.CabalFile
import com.haskforce.cabal.query.{BuildInfo, CabalQuery, Named}
import com.haskforce.settings.{HaskellBuildSettings, ToolKey}
import com.haskforce.ui.tools.HaskellToolsConsole
import com.haskforce.utils.CastUtil

final class GhciProjectComponent(project: Project) extends ProjectComponent {

  private val LOG = Logger.getInstance(classOf[GhciProjectComponent])

  override def initComponent(): Unit = {}

  override def disposeComponent(): Unit = {
    instances.values().iterator().asScala.foreach { ghci =>
      Ghci.unsafeKill(ghci)
    }
    instances.clear()
  }

  override def projectOpened(): Unit = {}

  override def projectClosed(): Unit = {}

  override def getComponentName: String = getClass.getSimpleName

  def get(file: PsiFile): Option[Ghci] = {
    (for {
      processInfo <- Ghci.ProcessInfo.create(file.getProject)
      module <- Option(ModuleUtilCore.findModuleForPsiElement(file)).disj(
        "Could not find module for PsiFile: " + file.getName
      )
      cabalVFile <- HaskellModuleType.findCabalFile(module).disj(
        "Could not find cabal file for Module: " + module.getName
      )
      workVDir <- Option(cabalVFile.getParent).disj(
        "Could not get directory of cabal file: " + cabalVFile.getPath
      )
      workDirPath <- Option(workVDir.getCanonicalPath).disj(
        "Could not get canonical directory path: " + workVDir.getPath
      )
      project = file.getProject
      psiMgr = PsiManager.getInstance(project)
      psiFile <- Option(psiMgr.findFile(cabalVFile)).disj(
        "Could not find PsiFile for VirtualFile: " + cabalVFile.getPath
      )
      cabalFile <- CastUtil.down[CabalFile](psiFile).disj(
        "PsiFile is not a CabalFile: " + psiFile.getName
      )
      q = new CabalQuery(cabalFile)
      buildInfo <- q.findBuildInfoForSourceFile(file).disj(
        "Could not find cabal build info for file: " + file.getName
      )
      k = Ghci.Key.create(buildInfo)
      toolsConsole = HaskellToolsConsole.get(project).curry(ToolKey.GHCI)
      ghci <- getOrCreate(Ghci.Args(k, project, workDirPath, buildInfo, processInfo, toolsConsole))
    } yield ghci).fold(
      msg => { LOG.warn(msg); None},
      Some(_)
    )
  }

  private val instances = new ConcurrentHashMap[Ghci.Key, Ghci]()

  private def getOrCreate(args: Ghci.Args): String \/ Ghci = {
    Option(instances.get(args.key)).map(\/.right).getOrElse {
      createProcess(args).map { p =>
        val ghci = new Ghci(args, p)
        instances.put(args.key, ghci)
        ghci
      }
    }
  }

  private def createProcess(args: Ghci.Args): String \/ Process = {
    val commandLine = new GeneralCommandLine(args.processInfo.executable)
    args.toolsConsole.writeInput("Using executable: " + args.workDir)
    commandLine.withWorkDirectory(args.workDir)
    args.toolsConsole.writeInput("Using working directory: " + args.workDir)
    commandLine.setRedirectErrorStream(true)
    val paramsList = commandLine.getParametersList
    paramsList.addParametersString(args.processInfo.flags)
    args.toolsConsole.writeInput("Using configured flags: " + args.processInfo.flags)
    val opts = allOpts(args.buildInfo).toArray
    paramsList.addAll(opts: _*)
    args.toolsConsole.writeInput("Using build flags: " + opts.mkString(" "))
    try {
      \/.right(commandLine.createProcess())
    } catch {
      case e: ExecutionException =>
        LOG.error(e)
        args.toolsConsole.writeError("Failed to initialize process: " + e.getMessage)
        \/.left("Could not create Ghci process: " + e.getMessage)
    }
  }

  private def allOpts(info: BuildInfo): Stream[String] = {
    extOpts(info) #::: pkgOpts(info) #::: ghcOpts(info)
  }

  private def extOpts(info: BuildInfo) = info.getExtensions.toStream.map("-X" + _)

  private def pkgOpts(info: BuildInfo) = {
    val s = info.getDependencies.toStream.flatMap(d => Array("-package", d))
    "-hide-all-packages" #:: s
  }

  private def ghcOpts(info: BuildInfo) = info.getGhcOptions.toStream
}

object Ghci {

  private val LOG = Logger.getInstance(classOf[Ghci])

  def unsafeKill(ghci: Ghci): Unit = ghci.kill()

  final case class Args(
    key: Key,
    project: Project,
    workDir: String,
    buildInfo: BuildInfo,
    processInfo: ProcessInfo,
    toolsConsole: HaskellToolsConsole.Curried
  )

  final case class ProcessInfo(
    executable: String,
    flags: String
  )

  object ProcessInfo {
    def create(project: Project): String \/ ProcessInfo = {
      val buildSettings = HaskellBuildSettings.getInstance(project)
      if (buildSettings.isStackEnabled) {
        val stackPath = buildSettings.getStackPath
        if (stackPath.isEmpty) return \/.left("Empty stack path")
        return \/.right(ProcessInfo(
          stackPath,
          buildSettings.getStackFlags + " ghc -- --interactive"
        ))
      } else if (buildSettings.isCabalEnabled) {
        val ghcPath = buildSettings.getGhcPath
        if (ghcPath.isEmpty) return \/.left("Empty ghc path")
        return \/.right(ProcessInfo(ghcPath, "--interactive"))
      }
      \/.left("Neither stack nor cabal enabled")
    }
  }

  final case class Key(typ: BuildInfo.Type, name: Option[String]) {
    /** Locate the BuildInfo for this Key. */
    def findBuildInfo(q: CabalQuery): Option[BuildInfo] = typ match {
      case BuildInfo.Type.Library => q.getLibrary
      case BuildInfo.Type.Executable => findNamed(q.getExecutables)
      case BuildInfo.Type.TestSuite => findNamed(q.getTestSuites)
      case BuildInfo.Type.Benchmark => findNamed(q.getBenchmarks)
    }

    /** Helper for locating the BuildInfo type with this Key's name. */
    private def findNamed[A <: Named](infos: Array[A]): Option[A] = {
      infos.find(_.getName == name)
    }
  }

  object Key {
    def create(info: BuildInfo): Key = info match {
      case _: BuildInfo.Library => Key(info.typ, None)
      case n: Named => Key(info.typ, n.getName)
    }
  }
}

final class Ghci(args: Ghci.Args, process: Process) {

  import args.toolsConsole

  def isAlive: Boolean = alive.get()

  private val alive = new AtomicBoolean(true)

  def exec(command: String): Option[String] = {
    if (!write(command)) return None
    read(command)
  }

  private def write(command: String): Boolean = {
    toolsConsole.writeInput(command)
    try {
      output.write(command)
      output.newLine()
      output.flush()
      true
    } catch {
      case e: IOException =>
        val msg = "Failed to write command to ghci: " + command
        toolsConsole.writeError(msg + "\n" + e)
        Ghci.LOG.error(msg, e)
        false
    }
  }

  private def read(command: String): Option[String] = {
    try {
      val lines = Stream.continually { input.readLine() }.takeWhile(_ != null)
      val joined = lines.mkString("\n")
      toolsConsole.writeOutput(joined)
      Some(joined)
    } catch {
      case e@(_: IOException | _: InterruptedException) =>
        val msg = "Failed to read from ghci for command: " + command
        toolsConsole.writeError(msg + "\n" + e)
        Ghci.LOG.error(msg, e)
        None
    }
  }

  private def kill(): Unit = this.synchronized {
    def ignoreIOException(f: => Unit): Unit = {
      try {
        f
      } catch {
        case e: IOException => // ignored
      }
    }
    ignoreIOException { process.destroy() }
    ignoreIOException { input.close() }
    ignoreIOException { output.close() }
    alive.set(false)
  }

  private val input = new BufferedReader(new InputStreamReader(process.getInputStream))
  private val output = new BufferedWriter(new OutputStreamWriter(process.getOutputStream))
}
