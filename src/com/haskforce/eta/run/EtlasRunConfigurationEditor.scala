package com.haskforce.eta.run

import javax.swing.JComponent

import com.intellij.openapi.options.SettingsEditor

class EtlasRunConfigurationEditor(
  runConfig: EtlasRunConfiguration
) extends SettingsEditor[EtlasRunConfiguration] {

  private val form = new EtlasRunConfigurationForm

  override def createEditor(): JComponent = form.mainPanel

  override def applyEditorTo(s: EtlasRunConfiguration): Unit = {
    s.updateConfigState(_ => EtlasRunConfigurationState(
      programArguments = form.programArguments.getText,
      workingDirectory = form.workingDirectory.getText,
      vmArguments = form.vmArguments.getText,
      environmentVariables = form.environmentVariables.getEnvData
    ))
  }

  override def resetEditorFrom(s: EtlasRunConfiguration): Unit = {
    val st = s.getConfigState
    form.programArguments.setText(st.programArguments)
    form.workingDirectory.setText(st.workingDirectory)
    form.vmArguments.setText(st.vmArguments)
    form.environmentVariables.setEnvData(st.environmentVariables)
  }
}
