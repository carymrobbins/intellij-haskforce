package com.haskforce.run.stack.task;

import com.intellij.execution.configuration.EnvironmentVariablesComponent;
import com.intellij.execution.configuration.EnvironmentVariablesData;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.ui.RawCommandLineEditor;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class StackTaskConfigurationEditorForm extends SettingsEditor<StackTaskConfiguration> {

    private JPanel mainPanel;
    private RawCommandLineEditor task;
    private EnvironmentVariablesComponent envVars;
    private JCheckBox useCurrentSSHAgentVars;

    @NotNull
    @Override
    protected JComponent createEditor() {
        return mainPanel;
    }

    @Override
    protected void resetEditorFrom(@NotNull StackTaskConfiguration config) {
        StackTaskConfigurationState st = config.getConfigState();
        task.setText(st.task());
        envVars.setEnvData(st.environmentVariables());
        useCurrentSSHAgentVars.setSelected(st.useCurrentSSHAgentVars());
    }

    @Override
    protected void applyEditorTo(@NotNull StackTaskConfiguration config) {
        config.updateConfigState(state -> state.copy(
            task.getText(),
            envVars.getEnvData(),
            useCurrentSSHAgentVars.isSelected()
        ));
    }
}
