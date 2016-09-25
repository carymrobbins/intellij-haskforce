package com.haskforce.system.settings;

import com.haskforce.system.packages.FileError;
import com.haskforce.system.packages.HPackageManager;
import com.haskforce.system.utils.ExecUtil;
import com.haskforce.system.utils.GuiUtil;
import com.haskforce.system.utils.NotificationUtil;
import com.haskforce.tools.cabal.packages.CabalPackageManager$;
import com.haskforce.tools.stack.packages.StackPackageManager$;
import com.intellij.compiler.options.CompilerConfigurable;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.Condition;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.ui.TextAccessor;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.collection.JavaConversions;
import scala.collection.immutable.List;
import scala.runtime.AbstractFunction1;
import scala.util.Either;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.io.File;
import java.util.Arrays;
import java.util.stream.Collectors;

/**
 * The "Haskell Compiler" section in Preferences->Compiler.
 */
public class HaskellCompilerConfigurable extends CompilerConfigurable {
    public static final String HASKELL_COMPILER_ID = "Haskell compiler";

    // Swing components.
    private JPanel mainPanel;
    // GHC Binary components.
    private TextFieldWithBrowseButton ghcPath;
    private JLabel ghcVersion;
    // Cabal binary components.
    private TextFieldWithBrowseButton cabalPath;
    private JLabel cabalVersion;
    // Cabal configure flags
    private com.intellij.ui.RawCommandLineEditor cabalFlags;
    // Build configuration components.
    private JCheckBox profilingBuild;
    private JCheckBox cabalSandbox;
    private JCheckBox installCabalDependencies;
    private JCheckBox enableTests;
    private JRadioButton buildWithCabal;
    private TextFieldWithBrowseButton stackPath;
    private JRadioButton buildWithStack;
    private JLabel stackVersion;
    private TextFieldWithBrowseButton stackFile;
    private com.intellij.ui.RawCommandLineEditor stackFlags;
    private ButtonGroup buildWith = new ButtonGroup();

    // Data container for settings.
    private final HaskellBuildSettings mySettings;

    @SuppressWarnings("FieldCanBeLocal")
    private final Project myProject;

    public HaskellCompilerConfigurable(@NotNull final Project inProject) {
        super(inProject);
        myProject = inProject;
        mySettings = HaskellBuildSettings.getInstance(myProject);

        stackPath.setText(mySettings.getStackPath());
        GuiUtil.addFolderListener(stackPath, "stack");
        stackFile.setText(mySettings.getStackFile());
        GuiUtil.addFolderListener(stackFile, "stack.yaml", inProject, new Condition<VirtualFile>() {
            @Override
            public boolean value(VirtualFile virtualFile) {
                String ext = virtualFile.getExtension();
                return ext != null && Arrays.asList("yaml", "yml").contains(ext.toLowerCase());
            }
        });

        ghcPath.setText(mySettings.getGhcPath());
        GuiUtil.addFolderListener(ghcPath, "ghc");

        cabalPath.setText(mySettings.getCabalPath());
        GuiUtil.addFolderListener(cabalPath, "cabal");

        cabalSandbox.setSelected(mySettings.isCabalSandboxEnabled());
        installCabalDependencies.setSelected(mySettings.isInstallCabalDependenciesEnabled());
        enableTests.setSelected(mySettings.isEnableTestsEnabled());

        initializeBuildWithButtons();
        updateVersionInfoFields();
    }

    private void initializeBuildWithButtons() {
        buildWith.add(buildWithStack);
        buildWith.add(buildWithCabal);

        boolean stackEnabled = mySettings.isStackEnabled();
        buildWithStack.setSelected(stackEnabled);
        setEnabledStackFields(stackEnabled);

        // Cabal and Stack can't be enabled simultaneously, prefer Stack.
        boolean cabalEnabled = !stackEnabled && mySettings.isCabalEnabled();
        buildWithCabal.setSelected(cabalEnabled);
        setEnabledCabalFields(cabalEnabled);

        buildWithStack.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setEnabledStackFields(true);
            }
        });

        buildWithCabal.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setEnabledCabalFields(true);
            }
        });
    }

    /**
     * The setEnabledStack/CabalFields methods will toggle the other fields
     * as enabled so that the Stack and Cabal fields won't be enabled simultaneously.
     */

    private void setEnabledStackFields(boolean enabled) {
        setEnabledStackFields(enabled, true);
    }

    private void setEnabledStackFields(boolean enabled, boolean toggle) {
        stackPath.setEnabled(enabled);
        stackFlags.setEnabled(enabled);
        stackFile.setEnabled(enabled);
        if (toggle) setEnabledCabalFields(!enabled, false);
    }

    private void setEnabledCabalFields(boolean enabled) {
        setEnabledCabalFields(enabled, true);
    }

    private void setEnabledCabalFields(boolean enabled, boolean toggle) {
        ghcPath.setEnabled(enabled);
        cabalPath.setEnabled(enabled);
        cabalFlags.setEnabled(enabled);
        profilingBuild.setEnabled(enabled);
        installCabalDependencies.setEnabled(enabled);
        cabalSandbox.setEnabled(enabled);
        enableTests.setEnabled(enabled);
        if (toggle) setEnabledStackFields(!enabled, false);
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
        return !(ghcCabalStackUnchanged() &&
                cabalFlags.getText().equals(mySettings.getCabalFlags()) &&
                profilingBuild.isSelected() == mySettings.isProfilingEnabled() &&
                buildWithCabal.isSelected() == mySettings.isCabalEnabled() &&
                cabalSandbox.isSelected() == mySettings.isCabalSandboxEnabled() &&
                installCabalDependencies.isSelected() == mySettings.isInstallCabalDependenciesEnabled() &&
                enableTests.isSelected() == mySettings.isEnableTestsEnabled() &&
                buildWithStack.isSelected() == mySettings.isStackEnabled() &&
                stackFlags.getText().equals(mySettings.getStackFlags()));
    }

    /**
     * Returns true if the ghc and cabal paths are unchanged.
     */
    private boolean ghcCabalStackUnchanged() {
        return ghcPath.getText().equals(mySettings.getGhcPath()) &&
                cabalPath.getText().equals(mySettings.getCabalPath()) &&
                stackPath.getText().equals(mySettings.getStackPath()) &&
                stackFile.getText().equals(mySettings.getStackFile());
    }

    /**
     * Triggered when the user pushes the apply button.
     */
    @Override
    public void apply() throws ConfigurationException {
        validate();
        String file;
        if (buildWithStack.isSelected()) {
            file = stackPath.getText();
        } else {
            file = cabalPath.getText();
        }
        setMainProject(buildWithStack.isSelected(), buildWithCabal.isSelected() && !buildWithStack.isSelected(), file, myProject);
        saveState();
        updateVersionInfoFields();
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
        mySettings.setUseCabal(buildWithCabal.isSelected() && !buildWithStack.isSelected());
        mySettings.setUseCabalSandbox(cabalSandbox.isSelected());
        mySettings.setInstallCabalDependencies(installCabalDependencies.isSelected());
        mySettings.setEnableTests(enableTests.isSelected());
        mySettings.setGhcPath(ghcPath.getText());
        mySettings.setCabalPath(cabalPath.getText());
        mySettings.setCabalFlags(cabalFlags.getText());
        mySettings.setUseStack(buildWithStack.isSelected());
        mySettings.setStackPath(stackPath.getText());
        mySettings.setStackFlags(stackFlags.getText());
        mySettings.setStackFile(stackFile.getText());
    }

    private void setMainProject(boolean stack, boolean cabal, String file, Project project) throws ConfigurationException {
        HPackageManager packageManager = project.getComponent(HPackageManager.class);
        if (stack) {
            Either<FileError, List<FileError>> result = packageManager.replaceMainPackage(StackPackageManager$.MODULE$, file);
            if (result.isLeft()) {
                throw new ConfigurationException("Unable to set Main Project to Stack. " + result.left().get().errorMsg());
            } else {
                List<FileError> fileErrorList = result.right().get();
                if (!fileErrorList.isEmpty()) {
                    String errorMessages = JavaConversions.seqAsJavaList(fileErrorList).stream()
                            .map(error -> "in package: " + error.fileName() + " error: " + error.errorMsg())
                            .collect(Collectors.joining("<br/>"));
                    NotificationUtil.displaySimpleNotification(
                            NotificationType.ERROR, myProject, "unable to set up all packages correctly", errorMessages
                    );
                }
            }
        } else if (cabal) {
            Either<FileError, List<FileError>> result = packageManager.replaceMainPackage(CabalPackageManager$.MODULE$, file);
            if (result.isLeft()) {
                throw new ConfigurationException("Unable to set Main Project to Cabal. " + result.left().get().errorMsg());
            } else {
                List<FileError> fileErrorList = result.right().get();
                if (!fileErrorList.isEmpty()) {
                    String errorMessages = JavaConversions.seqAsJavaList(fileErrorList).stream()
                            .map(error -> "in package: " + error.fileName() + " error: " + error.errorMsg())
                            .collect(Collectors.joining("<br/>"));
                    NotificationUtil.displaySimpleNotification(
                            NotificationType.ERROR, myProject, "unable to set up all packages correctly", errorMessages
                    );
                }
            }
        }
    }

    private void validate() throws ConfigurationException {
        if (buildWithCabal.isSelected()) {
            validateExecutable("cabal", cabalPath);
            validateExecutable("ghc", ghcPath);
        }
        if (buildWithStack.isSelected()) {
            validateExecutable("stack", stackPath);
            validateFileExists("stack.yaml", stackFile);
        }
    }

    private void validateExecutable(String name, TextAccessor field) throws ConfigurationException {
        if (new File(field.getText()).canExecute() || new File(myProject.getBasePath(), field.getText()).exists()) return;
        throw new ConfigurationException("Not a valid '" + name + "' executable: '" + field.getText() + "'");
    }

    private void validateFileExists(String name, TextAccessor field) throws ConfigurationException {
        if (new File(field.getText()).exists() || new File(myProject.getBasePath(), field.getText()).exists()) return;
        throw new ConfigurationException("'" + name + "' file does not exist: '" + field.getText() + "'");
    }

    /**
     * Updates the version info fields for all files configured.
     */
    private void updateVersionInfoFields() {
        updateVersionInfoField("ghc", ghcPath.getText(), "--numeric-version", ghcVersion);
        updateVersionInfoField("cabal", cabalPath.getText(), "--numeric-version", cabalVersion);
        updateVersionInfoField("stack", stackPath.getText(), "--numeric-version", stackVersion);
    }

    private void updateVersionInfoField(final String name, String exePath, String versionFlag,
                                        final JLabel versionField) {
        ExecUtil.readCommandLine(null, exePath, versionFlag).fold(
            new AbstractFunction1<ExecUtil.ExecError, Void>() {
                @Override
                public Void apply(ExecUtil.ExecError e) {
                    NotificationUtil.displaySimpleNotification(
                        NotificationType.ERROR, myProject, name, e.getMessage()
                    );
                    return null;
                }
            },
            new AbstractFunction1<String, Void>() {
                @Override
                public Void apply(String version) {
                    versionField.setText(version);
                    return null;
                }
            }
        );
    }

    /**
     * Restore components to the initial state.
     */
    private void restoreState() {
        ghcPath.setText(mySettings.getGhcPath());
        cabalPath.setText(mySettings.getCabalPath());
        cabalFlags.setText(mySettings.getCabalFlags());
        profilingBuild.setSelected(mySettings.isProfilingEnabled());
        buildWithCabal.setSelected(mySettings.isCabalEnabled());
        cabalSandbox.setSelected(mySettings.isCabalSandboxEnabled());
        installCabalDependencies.setSelected(mySettings.isInstallCabalDependenciesEnabled());
        enableTests.setSelected(mySettings.isEnableTestsEnabled());
        buildWithStack.setSelected(mySettings.isStackEnabled());
        stackPath.setText(mySettings.getStackPath());
        stackFlags.setText(mySettings.getStackFlags());
        stackFile.setText(mySettings.getStackFile());
    }
}
