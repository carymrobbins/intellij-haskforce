package com.haskforce.tooling.ghcPkg

import java.io.InputStream

import com.intellij.execution.configurations.GeneralCommandLine

object GhcPkgDumpExecutor {

  def runWithStack(stackPath: String, workDir: String): InputStream = {
    val c = new GeneralCommandLine(stackPath, "exec", "ghc-pkg", "dump")
    c.setWorkDirectory(workDir)
    val p = c.createProcess()
    p.getInputStream
  }
}
