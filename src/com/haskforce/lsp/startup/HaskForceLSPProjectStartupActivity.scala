package com.haskforce.lsp.startup

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
    val hlsPath = getHLSPath()
    val processBuilder = new ProcessBuilder(hlsPath, "--lsp", "--cwd", workDir)
    val serverDef = new ProcessBuilderServerDefinition("hs", processBuilder)
    IntellijLanguageClient.addServerDefinition(serverDef, project)
  }

  // TODO: Make this configurable
  private def getHLSPath(): String = {
    val home = System.getProperty("user.home")
    if (home == null) {
      throw new RuntimeException("Failed to guess HLS install dir; user.home was null")
    }
    home + "/.local/bin/haskell-language-server"
  }
}

object HaskForceLSPProjectStartupActivity {
  private val LOG = Logger.getInstance(getClass)
}
