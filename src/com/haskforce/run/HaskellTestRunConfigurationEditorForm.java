package com.haskforce.run;

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

/**
 * The run configuration form in Run->Edit Configurations->Haskell
 */
public class HaskellTestRunConfigurationEditorForm extends SettingsEditor<HaskellTestRunConfiguration> {
    private JPanel mainPanel;
    private com.intellij.ui.RawCommandLineEditor programArguments;

    @Override
    protected void resetEditorFrom(HaskellTestRunConfiguration haskellRunConfiguration) {
        programArguments.setText(haskellRunConfiguration.programArguments);
    }

    @Override
    protected void applyEditorTo(HaskellTestRunConfiguration haskellRunConfiguration) throws ConfigurationException {
        haskellRunConfiguration.programArguments = programArguments.getText();
    }

    @NotNull
    @Override
    protected JComponent createEditor() {
        return mainPanel;
    }
}
