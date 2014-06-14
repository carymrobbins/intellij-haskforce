package com.haskforce.settings;

import com.haskforce.HaskellSdkType;
import com.haskforce.jps.model.HaskellBuildOptions;
import com.haskforce.utils.ExecUtil;
import com.intellij.compiler.options.CompilerConfigurable;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.project.Project;
import com.intellij.ui.TextFieldWithHistory;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.io.File;

/**
 * The "Haskell Compiler" section in Preferences->Compiler.
 */
public class HaskellCompilerConfigurable extends CompilerConfigurable {
    public static final String HASKELL_COMPILER_ID = "Haskell compiler";

    // Swing components.
    private JPanel mainPanel;
    // GHC Binary components.
    private com.intellij.openapi.ui.TextFieldWithBrowseButton ghcPath;
    private JLabel ghcVersion;
    // Cabal binary components.
    private com.intellij.openapi.ui.TextFieldWithBrowseButton cabalPath;
    private JLabel cabalVersion;
    // Cabal configure flags
    private TextFieldWithHistory cabalFlags;
    // Build configuration components.
    private JCheckBox profilingBuild;
    private JCheckBox cabalBuild;
    private JCheckBox cabalSandbox;
    private JCheckBox installCabalDependencies;

    // Data container for settings.
    private final HaskellBuildSettings mySettings;
    // Improved settings if default values.
    private String bestGhcPath;
    private String bestCabalPath;

    private final Project myProject;

    public HaskellCompilerConfigurable(@NotNull Project inProject) {
        super(inProject);
        myProject = inProject;
        mySettings = HaskellBuildSettings.getInstance(myProject);
        bestGhcPath = mySettings.getGhcPath();
        bestCabalPath = mySettings.getCabalPath();
        String sdkPath = HaskellSdkType.getHaskellSdkPath(myProject);
        File sdkGhcPath = sdkPath == null ? null : HaskellSdkType.getExecutable(sdkPath);
        String foundCabalPath = ExecUtil.locateExecutable(HaskellBuildOptions.DEFAULT_CABAL_PATH);

        if (sdkGhcPath != null && bestGhcPath.equals(HaskellBuildOptions.DEFAULT_GHC_PATH)) {
            bestGhcPath = sdkGhcPath.getAbsolutePath();
        }
        ghcPath.setText(bestGhcPath);

        if (foundCabalPath != null && !foundCabalPath.isEmpty() &&
                bestCabalPath.equals(HaskellBuildOptions.DEFAULT_CABAL_PATH)) {
            bestCabalPath = foundCabalPath;
        }
        cabalPath.setText(bestCabalPath);

        cabalBuild.setSelected(mySettings.isCabalEnabled());
        cabalSandbox.setSelected(mySettings.isCabalSandboxEnabled());
        installCabalDependencies.setSelected(mySettings.isInstallCabalDependenciesEnabled());
        updateVersionInfoFields();
    }

    @NotNull
    @Override
    public String getId() {
        return HASKELL_COMPILER_ID;
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

    /**
     * Constructs the compiler panel in Settings->Compiler. Also responsible
     * for filling in previous values or constructing sane default values.
     */
    @Nullable
    @Override
    public JComponent createComponent() {
        return mainPanel;
    }

    /**
     * Enables the apply button if anything changed.
     */
    @Override
    public boolean isModified() {
        return !(ghcAndCabalUnchanged() &&
                cabalFlags.getText().equals(mySettings.getCabalFlags()) &&
                profilingBuild.isSelected() == mySettings.isProfilingEnabled() &&
                cabalBuild.isSelected() == mySettings.isCabalEnabled() &&
                cabalSandbox.isSelected() == mySettings.isCabalSandboxEnabled() &&
                installCabalDependencies.isSelected() == mySettings.isInstallCabalDependenciesEnabled());
    }

    /**
     * Returns true if the ghc and cabal paths are unchanged.
     */
    private boolean ghcAndCabalUnchanged() {
        return (ghcPath.getText().equals(mySettings.getGhcPath()) ||
                ghcPath.getText().equals(bestGhcPath)) &&
                (cabalPath.getText().equals(mySettings.getCabalPath()) ||
                        cabalPath.getText().equals(bestCabalPath));
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
        mySettings.setInstallCabalDependencies(installCabalDependencies.isSelected());
        bestGhcPath = ghcPath.getText();
        mySettings.setGhcPath(bestGhcPath);
        bestCabalPath = cabalPath.getText();
        mySettings.setCabalPath(bestCabalPath);
        mySettings.setCabalFlags(cabalFlags.getText());
    }

    /**
     * Updates the version info fields for all files configured.
     */
    private void updateVersionInfoFields() {
        ghcVersion.setText(ExecUtil.exec(ghcPath.getText() +  " --numeric-version"));
        cabalVersion.setText(ExecUtil.exec(cabalPath.getText() +  " --numeric-version"));
    }

    /**
     * Restore components to the initial state.
     */
    private void restoreState() {
        ghcPath.setText(bestGhcPath);
        cabalPath.setText(bestCabalPath);
        cabalFlags.setText(mySettings.getCabalFlags());
        profilingBuild.setSelected(mySettings.isProfilingEnabled());
        cabalBuild.setSelected(mySettings.isCabalEnabled());
        cabalSandbox.setSelected(mySettings.isCabalSandboxEnabled());
        installCabalDependencies.setSelected(mySettings.isInstallCabalDependenciesEnabled());
    }
}
