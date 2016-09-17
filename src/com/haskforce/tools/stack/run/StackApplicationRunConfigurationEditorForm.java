package com.haskforce.tools.stack.run;


import com.haskforce.haskell.run.stack.StackApplicationRunConfiguration;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import com.intellij.ui.RawCommandLineEditor;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class StackApplicationRunConfigurationEditorForm extends SettingsEditor<StackApplicationRunConfiguration> {

    private JPanel mainPanel;
    private RawCommandLineEditor executable;
    private RawCommandLineEditor executableArguments;


    @NotNull
    @Override
    protected JComponent createEditor() {
        return mainPanel;
    }

    @Override
    protected void resetEditorFrom(StackApplicationRunConfiguration config) {
        executable.setText(config.executable());
        executableArguments.setText(config.executableArguments());
    }

    @Override
    protected void applyEditorTo(StackApplicationRunConfiguration config) throws ConfigurationException {
        config.setExecutable(executable.getText());
        config.setExecutableArguments(executableArguments.getText());
    }
}
