package com.haskforce.eta.run

import com.haskforce.eta.settings.EtaBuildSettings
import com.intellij.execution.configurations.{CommandLineState, GeneralCommandLine}
import com.intellij.execution.process.{OSProcessHandler, ProcessHandler}
import com.intellij.execution.runners.ExecutionEnvironment

class EtlasCommandLineState(env: ExecutionEnvironment, rc: EtlasRunConfiguration)
  extends CommandLineState(env) {

  override def startProcess(): ProcessHandler = {
    val buildSettings = EtaBuildSettings.getInstance(rc.getProject)
    val st = rc.getConfigState
    val cmd = new GeneralCommandLine
    cmd.setWorkDirectory(st.workingDirectory)
    cmd.setExePath(buildSettings.getEtlasPath)
    val params = cmd.getParametersList
    params.addParametersString(st.programArguments)
    params.add("--java-options=" + st.vmArguments)
    cmd.withEnvironment(st.environmentVariables.getEnvs)
    new OSProcessHandler(cmd)
  }
}
