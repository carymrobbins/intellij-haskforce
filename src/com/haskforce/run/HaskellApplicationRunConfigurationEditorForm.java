package com.haskforce.run;

import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SettingsEditor;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

/**
 * The run configuration form in Run->Edit Configurations->Haskell
 */
public class HaskellApplicationRunConfigurationEditorForm extends SettingsEditor<HaskellApplicationRunConfiguration> {
    private JPanel mainPanel;
    private com.intellij.ui.RawCommandLineEditor programArguments;

    @Override
    protected void resetEditorFrom(HaskellApplicationRunConfiguration haskellRunConfiguration) {
        programArguments.setText(haskellRunConfiguration.programArguments);
    }

    @Override
    protected void applyEditorTo(HaskellApplicationRunConfiguration haskellRunConfiguration) throws ConfigurationException {
        haskellRunConfiguration.programArguments = programArguments.getText();
    }

    @NotNull
    @Override
    protected JComponent createEditor() {
        return mainPanel;
    }
}
