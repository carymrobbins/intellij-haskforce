package com.haskforce.settings;

import com.haskforce.utils.ExecUtil;
import com.haskforce.utils.GuiUtil;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
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

    private Project project;

    // Swing components.
    private JPanel mainPanel;
    private TextFieldWithBrowseButton parserHelperPath;
    private JLabel parserHelperVersion;
    private JButton parserHelperAutoFind;
    private TextFieldWithBrowseButton stylishPath;
    private JButton stylishAutoFind;
    private JLabel stylishVersion;
    private TextFieldWithBrowseButton hlintPath;
    private JButton hlintAutoFind;
    private JLabel hlintVersion;
    private TextFieldWithBrowseButton ghcModPath;
    private JButton ghcModAutoFind;
    private JLabel ghcModVersion;

    private Tool[] tools;

    public HaskellToolsConfigurable(@NotNull Project project) {
        this.project = project;
        this.propertiesComponent = PropertiesComponent.getInstance(project);
        tools = new Tool[]{
                new Tool(project, "parser-helper", "parserHelperPath", parserHelperPath, parserHelperAutoFind, parserHelperVersion, "--numeric-version"),
                new Tool(project, "stylish-haskell", "stylishPath", stylishPath, stylishAutoFind, stylishVersion),
                new Tool(project, "hlint", "hlintPath", hlintPath, hlintAutoFind, hlintVersion),
                new Tool(project, "ghc-mod", "ghcModPath", ghcModPath, ghcModAutoFind, ghcModVersion),
        };
    }

    class Tool {
        public final Project project;
        public final String command;
        public final String key;
        public String oldPath;
        public final TextFieldWithBrowseButton pathField;
        public final JLabel versionField;
        public final String versionParam;
        public final JButton autoFindButton;

        Tool(Project project, String command, String key, TextFieldWithBrowseButton pathField, JButton autoFindButton,
             JLabel versionField) {
            this(project, command, key, pathField, autoFindButton, versionField, "--version");
        }

        Tool(Project project, String command, String key, TextFieldWithBrowseButton pathField, JButton autoFindButton,
             JLabel versionField, String versionParam) {
            this.project = project;
            this.command = command;
            this.key = key;
            this.oldPath = propertiesComponent.getValue(key, "");
            this.pathField = pathField;
            this.versionField = versionField;
            this.versionParam = versionParam;
            this.autoFindButton = autoFindButton;

            if (!oldPath.isEmpty()) {
                pathField.setText(oldPath);
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
            return !pathField.getText().equals(oldPath);
        }

        private void saveState() {
            propertiesComponent.setValue(key, oldPath = pathField.getText());
        }

        private void restoreState() {
            pathField.setText(oldPath);
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
