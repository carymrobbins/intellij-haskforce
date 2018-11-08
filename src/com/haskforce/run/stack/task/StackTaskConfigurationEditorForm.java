package com.haskforce.run.stack.task;

import com.intellij.openapi.options.SettingsEditor;
import com.intellij.ui.RawCommandLineEditor;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class StackTaskConfigurationEditorForm extends SettingsEditor<StackTaskConfiguration> {

    private JPanel mainPanel;
    private RawCommandLineEditor task;
    private com.intellij.execution.configuration.EnvironmentVariablesTextFieldWithBrowseButton envVars;

    @NotNull
    @Override
    protected JComponent createEditor() {
        return mainPanel;
    }

    @Override
    protected void resetEditorFrom(@NotNull StackTaskConfiguration config) {
        task.setText(config.task());
    }

    @Override
    protected void applyEditorTo(@NotNull StackTaskConfiguration config) {
        config.setTask(task.getText());
        config.setEnvs(envVars.getEnvs());
    }
}
