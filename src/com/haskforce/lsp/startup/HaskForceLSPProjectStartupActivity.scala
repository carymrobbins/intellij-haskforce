package com.haskforce.lsp.startup

import java.io.File
import java.util

import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.openapi.startup.StartupActivity
import org.wso2.lsp4intellij.IntellijLanguageClient
import org.wso2.lsp4intellij.client.languageserver.serverdefinition.ProcessBuilderServerDefinition

class HaskForceLSPProjectStartupActivity extends StartupActivity {

  import HaskForceLSPProjectStartupActivity.LOG

  override def runActivity(project: Project): Unit = {
    val workDir = project.getBasePath
    if (workDir == null) {
      LOG.debug("Not spawning LSP for the default project")
      return
    }
    val processBuilder = mkProcessBuilder(workDir)
    val serverDef = new ProcessBuilderServerDefinition("hs", processBuilder)
    IntellijLanguageClient.addServerDefinition(serverDef, project)
  }

  private def mkProcessBuilder(workDir: String): ProcessBuilder = {
    val args = new util.ArrayList[String](5)
    args.add(getHLSPath())
    args.add("--lsp")
    args.add("--cwd")
    args.add(workDir)
    args.add("--logfile")
    args.add(getLogFile(workDir))
    if (debugEnabled()) args.add("--debug")
    new ProcessBuilder(args)
  }

  // TODO: Make this configurable
  private def getHLSPath(): String = {
    val home = System.getProperty("user.home")
    if (home == null) {
      throw new RuntimeException("Failed to guess HLS install dir; user.home was null")
    }
    home + "/.local/bin/haskell-language-server"
  }

  // TODO: Make this configurable
  private def getLogFile(workDir: String): String = {
    workDir + File.separator + "hls.log"
  }

  // TODO: Make this configurable
  private def debugEnabled(): Boolean = true
}

object HaskForceLSPProjectStartupActivity {
  private val LOG = Logger.getInstance(getClass)
}
