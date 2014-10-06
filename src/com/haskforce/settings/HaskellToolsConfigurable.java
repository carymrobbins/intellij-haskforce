package com.haskforce.settings;

import com.haskforce.utils.ExecUtil;
import com.haskforce.utils.GuiUtil;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.ui.RawCommandLineEditor;
import com.intellij.ui.TextFieldWithHistory;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

/**
 * The "Haskell Tools" option in Preferences->Project Settings.
 */
public class HaskellToolsConfigurable implements SearchableConfigurable {
    public static final String HASKELL_TOOLS_ID = "Haskell Tools";

    private PropertiesComponent propertiesComponent;

    // Swing components.
    private JPanel mainPanel;
    private TextFieldWithBrowseButton parserHelperPath;
    private RawCommandLineEditor parserHelperFlags;
    private JLabel parserHelperVersion;
    private JButton parserHelperAutoFind;
    private TextFieldWithBrowseButton stylishPath;
    private RawCommandLineEditor stylishFlags;
    private JButton stylishAutoFind;
    private JLabel stylishVersion;
    private TextFieldWithBrowseButton hlintPath;
    private RawCommandLineEditor hlintFlags;
    private JButton hlintAutoFind;
    private JLabel hlintVersion;
    private TextFieldWithBrowseButton ghcModPath;
    private RawCommandLineEditor ghcModFlags;
    private JButton ghcModAutoFind;
    private JLabel ghcModVersion;

    private Tool[] tools;

    public HaskellToolsConfigurable(@NotNull Project project) {
        this.propertiesComponent = PropertiesComponent.getInstance(project);
        tools = new Tool[]{
                new Tool(project, "parser-helper", ExecUtil.PARSER_HELPER_PATH_KEY, parserHelperPath, parserHelperFlags,
                        parserHelperAutoFind, parserHelperVersion, "--numeric-version"),
                new Tool(project, "stylish-haskell", ExecUtil.STYLISH_HASKELL_PATH_KEY, stylishPath, stylishFlags,
                        stylishAutoFind, stylishVersion),
                new Tool(project, "hlint", ExecUtil.HLINT_PATH_KEY, hlintPath, hlintFlags,
                        hlintAutoFind, hlintVersion),
                new Tool(project, "ghc-mod", ExecUtil.GHC_MOD_PATH_KEY, ghcModPath, ghcModFlags,
                        ghcModAutoFind, ghcModVersion, "version"),
        };
    }

    class Tool {
        public final Project project;
        public final String command;
        public final String pathKey;
        public final String flagsKey;
        public String oldPath;
        public String oldFlags;
        public final TextFieldWithBrowseButton pathField;
        public final RawCommandLineEditor flagsField;
        public final JLabel versionField;
        public final String versionParam;
        public final JButton autoFindButton;

        Tool(Project project, String command, ExecUtil.PathKey key, TextFieldWithBrowseButton pathField,
             RawCommandLineEditor flagsField, JButton autoFindButton, JLabel versionField) {
            this(project, command, key, pathField, flagsField, autoFindButton, versionField, "--version");
        }

        Tool(Project project, String command, ExecUtil.PathKey key, TextFieldWithBrowseButton pathField,
             RawCommandLineEditor flagsField, JButton autoFindButton, JLabel versionField, String versionParam) {
            this.project = project;
            this.command = command;
            this.pathField = pathField;
            this.flagsField = flagsField;
            this.versionField = versionField;
            this.versionParam = versionParam;
            this.autoFindButton = autoFindButton;

            this.pathKey = key.pathKey;
            this.oldPath = propertiesComponent.getValue(this.pathKey, "");
            if (!oldPath.isEmpty()) {
                pathField.setText(oldPath);
            }

            this.flagsKey = key.pathKey + "Flags";
            this.oldFlags = propertiesComponent.getValue(this.flagsKey, "");
            if (!oldFlags.isEmpty()) {
                pathField.setText(oldFlags);
            }

            GuiUtil.addFolderListener(pathField, command);
            GuiUtil.addApplyPathAction(autoFindButton, pathField, command);
            updateVersion();
        }

        public void updateVersion() {
            String pathText = pathField.getText();
            if (!pathText.isEmpty()) {
                versionField.setText(getVersion(pathText, versionParam));
            }
        }

        public boolean isModified() {
            return !(pathField.getText().equals(oldPath) || flagsField.getText().equals(oldFlags));
        }

        private void saveState() {
            propertiesComponent.setValue(pathKey, oldPath = pathField.getText());
            propertiesComponent.setValue(flagsKey, oldFlags = flagsField.getText());
        }

        private void restoreState() {
            pathField.setText(oldPath);
            flagsField.setText(oldFlags);
        }
    }

    @NotNull
    @Override
    public String getId() {
        return HASKELL_TOOLS_ID;
    }

    @Nullable
    @Override
    public Runnable enableSearch(String s) {
        // TODO
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
        return mainPanel;
    }

    /**
     * Enables the apply button if anything changed.
     */
    @Override
    public boolean isModified() {
        for (Tool tool : tools) {
            if (tool.isModified()) {
                return true;
            }
        }
        return false;
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
     * Heuristically finds the version number. Current implementation is the
     * identity function since cabal plays nice.
     */
    private static String getVersion(String cmd, String versionFlag) {
        return ExecUtil.exec(cmd + ' ' + versionFlag);
    }

    /**
     * Updates the version info fields for all files configured.
     */
    private void updateVersionInfoFields() {
        for (Tool tool : tools) {
            tool.updateVersion();
        }
    }

    /**
     * Persistent save of the current state.
     */
    private void saveState() {
        for (Tool tool : tools) {
            tool.saveState();
        }
    }

    /**
     * Restore components to the initial state.
     */
    private void restoreState() {
        for (Tool tool : tools) {
            tool.restoreState();
        }
    }
}
