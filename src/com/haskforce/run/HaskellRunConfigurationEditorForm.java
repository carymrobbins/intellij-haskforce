package com.haskforce.run;

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class HaskellRunConfigurationEditorForm extends SettingsEditor<HaskellRunConfiguration> {
    private JPanel mainPanel;
    private com.intellij.ui.RawCommandLineEditor programArguments;

    @Override
    protected void resetEditorFrom(HaskellRunConfiguration haskellRunconfiguration) {
        programArguments.setText(haskellRunconfiguration.programArguments);
    }

    @Override
    protected void applyEditorTo(HaskellRunConfiguration haskellRunConfiguration) throws ConfigurationException {
        haskellRunConfiguration.programArguments = programArguments.getText();
    }

    @NotNull
    @Override
    protected JComponent createEditor() {
        return mainPanel;
    }
}
