package com.haskforce.settings;


import com.haskforce.utils.ExecUtil;
import com.intellij.ide.util.PropertiesComponent;
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

    // Old values to detect user updates.
    private String oldCabalPath;

    // Swing components.
    private JPanel settings;
    private TextFieldWithBrowseButton cabalPath;
    private JLabel cabalVersion;

    public HaskellToolsConfigurable(@NotNull Project inProject) {
        project = inProject;
        oldCabalPath = PropertiesComponent.getInstance(project).getValue("cabalPath", "");
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

        // Cabal configuration.
        cabalPath = createExecutableOption("Cabal");
        cabalVersion = createDisplayVersion("Cabal");
        if (!oldCabalPath.isEmpty()) {
            cabalPath.setText(oldCabalPath);
            updateVersionInfoFields();
        }

        return settings;
    }

    /**
     * Enables the apply button if anything changed.
     */
    @Override
    public boolean isModified() {
        return !cabalPath.getText().equals(oldCabalPath);
    }

    /**
     * Triggered when the user pushes the apply button.
     */
    @Override
    public void apply() throws ConfigurationException {
        updateVersionInfoFields();
        saveState();
    }

    /**
     * Triggered when the user pushes the cancel button.
     */
    @Override
    public void reset() {
        restoreState();
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
        settings.add(new JLabel(tool + " executable path:"));
        settings.add(tf, ExternalSystemUiUtil.getFillLineConstraints(0));

        return tf;
    }

    /**
     * Creates two labels adds them to the configuration window.
     *
     * @param tool Which tool to display version for.
     * @return The label with dynamic content.
     */
    private JLabel createDisplayVersion(String tool) {
        JLabel tf = new JLabel("");

        // Add elements to Panel.
        settings.add(new JLabel(tool + " version:"));
        settings.add(tf, ExternalSystemUiUtil.getFillLineConstraints(0));

        return tf;
    }

    /**
     * Heuristically finds the version number. Current implementation is the
     * identity function since cabal plays nice.
     */
    private static String getVersion(String cmd, String versionflag) {
        return ExecUtil.run(cmd + ' ' + versionflag);
    }

    /**
     * Updates the version info fields for all files configured.
     */
    private void updateVersionInfoFields() {
        cabalVersion.setText(getVersion(cabalPath.getText(), "--numeric-version"));
    }

    /**
     * Persistent save of the current state.
     */
    private void saveState() {
        PropertiesComponent.getInstance(project).setValue("cabalPath", cabalPath.getText());
    }

    /**
     * Restore components to the initial state.
     */
    private void restoreState() {
        cabalPath.setText(oldCabalPath);
    }
}
