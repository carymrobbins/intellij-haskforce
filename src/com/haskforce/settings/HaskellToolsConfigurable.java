package com.haskforce.settings;


import com.intellij.openapi.externalSystem.util.ExternalSystemUiUtil;
import com.intellij.openapi.fileChooser.FileChooserDescriptorFactory;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;

/**
 * The "Haskell Tools" option in Preferences->Project Settings.
 */
public class HaskellToolsConfigurable implements SearchableConfigurable {
    public static final String HASKELL_TOOLS_ID = "Haskell Tools";

    private Project project;

    private JPanel settings;
    private TextFieldWithBrowseButton cabalPath;

    public HaskellToolsConfigurable(@NotNull Project inProject) {
        project = inProject;
    }

    @NotNull
    @Override
    public String getId() {
        return HASKELL_TOOLS_ID;
    }

    @Nullable
    @Override
    public Runnable enableSearch(String s) {
        return null;
    }

    @Nls
    @Override
    public String getDisplayName() {
        return HASKELL_TOOLS_ID;
    }

    @Nullable
    @Override
    public String getHelpTopic() {
        return null;
    }

    @Nullable
    @Override
    public JComponent createComponent() {
        settings = new JPanel(new GridBagLayout());
        cabalPath = createExecutableOption("Cabal");
        return settings;
    }

    @Override
    public boolean isModified() {
        return false;
    }

    /**
     * Triggered when the user pushes the apply button.
     */
    @Override
    public void apply() throws ConfigurationException {

    }

    /**
     * Triggered when the user pushes the cancel button.
     */
    @Override
    public void reset() {

    }

    @Override
    public void disposeUIResources() {

    }

    /**
     * Creates a label and path selector and adds them to the configuration
     * window.
     *
     * @param tool Which tool to configure.
     * @return The TextFieldWithBrowseButton created.
     */
    private TextFieldWithBrowseButton createExecutableOption(String tool) {
        // Create UI elements.
        TextFieldWithBrowseButton tf = new TextFieldWithBrowseButton();
        tf.addBrowseFolderListener("Select " + tool + " path", "", null,
                FileChooserDescriptorFactory.createSingleLocalFileDescriptor());

        // Add elements to Panel.
        settings.add(new JLabel(tool + " executable path"));
        settings.add(tf, ExternalSystemUiUtil.getFillLineConstraints(0));

        return tf;
    }
}
