package com.haskforce.tooling.ghcPkg

import java.io.InputStream

import com.intellij.execution.configurations.GeneralCommandLine

class GhcPkgDumpExecutor(
  workDir: String,
  stackExePath: String,
  stackYamlPath: String
) {

  def run(): CachedPkgs = {
    CachedPkgs.fromIterator(GhcPkgDumpParser.parse(streamViaStack()))
  }

  private def streamViaStack(): InputStream = {
    val c = new GeneralCommandLine(
      stackExePath, "--stack-yaml", stackYamlPath, "exec", "ghc-pkg", "dump"
    )
    c.setWorkDirectory(workDir)
    val p = c.createProcess()
    p.getInputStream
  }
}
