package com.haskforce.settings;

import com.haskforce.utils.ExecUtil;
import com.intellij.compiler.options.CompilerConfigurable;
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

    // Swing components.
    private JPanel settings;
    // GHC Binary components.
    private TextFieldWithBrowseButton ghcPath;
    private JLabel ghcVersion;
    // Cabal binary components
    private TextFieldWithBrowseButton cabalPath;
    private JLabel cabalVersion;
    // Build configuration components.
    private JCheckBox profilingBuild;
    private JCheckBox cabalBuild;
    private JCheckBox cabalSandbox;

    // Data container for settings.
    private final HaskellBuildSettings mySettings;

    public HaskellCompilerConfigurable(@NotNull Project inProject) {
        super(inProject);
        mySettings = HaskellBuildSettings.getInstance(inProject);
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
        ghcPath.setText(mySettings.getGhcPath());

        // Cabal path configuration.
        cabalPath = createExecutableOption(settings, "Cabal");
        cabalVersion = createDisplayVersion(settings, "Cabal");
        cabalPath.setText(mySettings.getCabalPath());

        // Build configuration.
        profilingBuild = createCheckBoxOption(settings, "Build with profiling information");
        cabalBuild = createCheckBoxOption(settings, "Build with Cabal");
        cabalSandbox = createCheckBoxOption(settings, "Build in Sandbox");
        cabalBuild.setSelected(mySettings.isCabalEnabled());
        cabalSandbox.setSelected(mySettings.isCabalSandboxEnabled());
        updateVersionInfoFields();

        return settings;
    }

    /**
     * Enables the apply button if anything changed.
     */
    @Override
    public boolean isModified() {
        return !(ghcPath.getText().equals(mySettings.getGhcPath()) &&
                cabalPath.getText().equals(mySettings.getCabalPath()) &&
                profilingBuild.isSelected() == mySettings.isProfilingEnabled() &&
                cabalBuild.isSelected() == mySettings.isCabalEnabled() &&
                cabalSandbox.isSelected() == mySettings.isCabalSandboxEnabled());
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
        // Save to disk and to communicate with build server.
        mySettings.setProfilingBuild(profilingBuild.isSelected());
        mySettings.setUseCabal(cabalBuild.isSelected());
        mySettings.setUseCabalSandbox(cabalSandbox.isSelected());
        mySettings.setGhcPath(ghcPath.getText());
        mySettings.setCabalPath(cabalPath.getText());
    }

    /**
     * Updates the version info fields for all files configured.
     */
    private void updateVersionInfoFields() {
        ghcVersion.setText(ExecUtil.run(ghcPath.getText() +  " --numeric-version"));
        cabalVersion.setText(ExecUtil.run(cabalPath.getText() +  " --numeric-version"));
    }

    /**
     * Restore components to the initial state.
     */
    private void restoreState() {
        ghcPath.setText(mySettings.getGhcPath());
        cabalPath.setText(mySettings.getCabalPath());
        profilingBuild.setSelected(mySettings.isProfilingEnabled());
        cabalBuild.setSelected(mySettings.isCabalEnabled());
        cabalSandbox.setSelected(mySettings.isCabalSandboxEnabled());
    }
}
