package com.haskforce.settings;

import com.haskforce.utils.ExecUtil;
import com.intellij.compiler.options.CompilerConfigurable;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;

import static com.haskforce.utils.GuiUtil.createCheckBoxOption;
import static com.haskforce.utils.GuiUtil.createDisplayVersion;
import static com.haskforce.utils.GuiUtil.createExecutableOption;

/**
 * The "Haskell Compiler" section in Preferences->Compiler.
 */
public class HaskellCompilerConfigurable extends CompilerConfigurable {
    public static final String HASKELL_COMPILER_ID = "Haskell compiler";

    private Project project;

    // Swing components.
    private JPanel settings;
    // GHC Binary
    private TextFieldWithBrowseButton ghcPath;
    private JLabel ghcVersion;
    // Build configuration
    private JCheckBox cabalBuild;
    private JCheckBox cabalSandbox;

    // Old values to detect user updates.
    private String oldGhcPath;
    private boolean oldCabalBuild;
    private boolean oldCabalSandbox;

    public HaskellCompilerConfigurable(@NotNull Project inProject) {
        super(inProject);
        project = inProject;

        PropertiesComponent state = PropertiesComponent.getInstance(project);

        oldGhcPath = state.getValue("ghcPath", "");
        oldCabalBuild = state.getBoolean("cabalBuild", true);
        oldCabalSandbox = state.getBoolean("cabalSandbox", false);
    }

    @NotNull
    @Override
    public String getId() {
        return HASKELL_COMPILER_ID ;
    }

    @Nullable
    @Override
    public Runnable enableSearch(String s) {
        return null;
    }

    @Nls
    @Override
    public String getDisplayName() {
        return HASKELL_COMPILER_ID;
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

        // GHC path configuration.
        ghcPath = createExecutableOption(settings, "GHC");
        ghcVersion = createDisplayVersion(settings, "GHC");
        if (!oldGhcPath.isEmpty()) {
            ghcPath.setText(oldGhcPath);
            updateVersionInfoFields();
        }

        // Build configuration.
        cabalBuild = createCheckBoxOption(settings, "Build with Cabal");
        cabalSandbox = createCheckBoxOption(settings, "Build in Sandbox");
        cabalBuild.setSelected(oldCabalBuild);
        cabalSandbox.setSelected(oldCabalSandbox);

        return settings;
    }

    /**
     * Enables the apply button if anything changed.
     */
    @Override
    public boolean isModified() {
        return !(ghcPath.getText().equals(oldGhcPath) &&
                cabalBuild.isSelected() == oldCabalBuild &&
                cabalSandbox.isSelected() == oldCabalSandbox);
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
     * Persistent save of the current state.
     */
    private void saveState() {
        PropertiesComponent state = PropertiesComponent.getInstance(project);

        state.setValue("ghcPath", ghcPath.getText());
        state.setValue("cabalBuild", String.valueOf(cabalBuild.isSelected()));
        state.setValue("cabalSandbox", String.valueOf(cabalSandbox.isSelected()));
    }

    /**
     * Updates the version info fields for all files configured.
     */
    private void updateVersionInfoFields() {
        ghcVersion.setText(ExecUtil.run(ghcPath.getText() +  " --numeric-version"));
    }

    /**
     * Restore components to the initial state.
     */
    private void restoreState() {
        ghcPath.setText(oldGhcPath);
        cabalBuild.setSelected(oldCabalBuild);
        cabalSandbox.setSelected(oldCabalSandbox);
    }
}
