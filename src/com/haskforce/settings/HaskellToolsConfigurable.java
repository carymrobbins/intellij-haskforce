package com.haskforce.settings;

import com.haskforce.utils.ExecUtil;
import com.haskforce.utils.GuiUtil;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.ui.RawCommandLineEditor;
import com.intellij.ui.TextAccessor;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.util.Arrays;
import java.util.List;

/**
 * The "Haskell Tools" option in Preferences->Project Settings.
 */
public class HaskellToolsConfigurable implements SearchableConfigurable {
    public static final String HASKELL_TOOLS_ID = "Haskell Tools";

    private PropertiesComponent propertiesComponent;

    // Swing components.
    private JPanel mainPanel;
    private TextFieldWithBrowseButton stylishPath;
    private RawCommandLineEditor stylishFlags;
    private JButton stylishAutoFind;
    private JTextField stylishVersion;
    private TextFieldWithBrowseButton hlintPath;
    private RawCommandLineEditor hlintFlags;
    private JButton hlintAutoFind;
    private JTextField hlintVersion;
    private TextFieldWithBrowseButton ghcModPath;
    private RawCommandLineEditor ghcModFlags;
    private JButton ghcModAutoFind;
    private JTextField ghcModVersion;

    private Tool[] tools;

    public HaskellToolsConfigurable(@NotNull Project project) {
        this.propertiesComponent = PropertiesComponent.getInstance(project);
        tools = new Tool[]{
                new Tool(project, "stylish-haskell", ExecUtil.STYLISH_HASKELL_KEY, stylishPath, stylishFlags,
                         stylishAutoFind, stylishVersion),
                new Tool(project, "hlint", ExecUtil.HLINT_KEY, hlintPath, hlintFlags,
                         hlintAutoFind, hlintVersion),
                new Tool(project, "ghc-mod", ExecUtil.GHC_MOD_KEY, ghcModPath, ghcModFlags,
                         ghcModAutoFind, ghcModVersion, "version"),
        };
    }

    class Tool {
        public final Project project;
        public final String command;
        public final ExecUtil.ToolKey key;
        public final TextFieldWithBrowseButton pathField;
        public final RawCommandLineEditor flagsField;
        public final JTextField versionField;
        public final String versionParam;
        public final JButton autoFindButton;
        public final List<PropertyField> propertyFields;

        Tool(Project project, String command, ExecUtil.ToolKey key, TextFieldWithBrowseButton pathField,
             RawCommandLineEditor flagsField, JButton autoFindButton, JTextField versionField) {
            this(project, command, key, pathField, flagsField, autoFindButton, versionField, "--version");
        }

        Tool(Project project, String command, ExecUtil.ToolKey key, TextFieldWithBrowseButton pathField,
             RawCommandLineEditor flagsField, JButton autoFindButton, JTextField versionField, String versionParam) {
            this.project = project;
            this.command = command;
            this.key = key;
            this.pathField = pathField;
            this.flagsField = flagsField;
            this.versionField = versionField;
            this.versionParam = versionParam;
            this.autoFindButton = autoFindButton;

            this.propertyFields = Arrays.asList(
                    new PropertyField(key.pathKey, pathField),
                    new PropertyField(key.flagsKey, flagsField));

            GuiUtil.addFolderListener(pathField, command);
            GuiUtil.addApplyPathAction(autoFindButton, pathField, command);
            updateVersion();
        }

        public void updateVersion() {
            String pathText = pathField.getText();
            if (pathText.isEmpty()) {
                versionField.setText("");
            } else {
                versionField.setText(getVersion(pathText, versionParam));
            }
        }

        public boolean isModified() {
            for (PropertyField propertyField : propertyFields) {
                if (propertyField.isModified()) {
                    return true;
                }
            }
            return false;
        }

        private void saveState() {
            for (PropertyField propertyField : propertyFields) {
                propertyField.saveState();
            }
        }

        private void restoreState() {
            for (PropertyField propertyField : propertyFields) {
                propertyField.restoreState();
            }
        }
    }

    class PropertyField {
        public String oldValue;
        public String propertyKey;
        public final TextAccessor field;

        PropertyField(@NotNull String propertyKey, @NotNull TextAccessor field) {
            this.propertyKey = propertyKey;
            this.field = field;
            this.oldValue = propertiesComponent.getValue(propertyKey, "");
            if (!oldValue.isEmpty()) {
                field.setText(oldValue);
            }
        }

        public boolean isModified() {
            return !field.getText().equals(oldValue);
        }

        public void saveState() {
            propertiesComponent.setValue(propertyKey, oldValue = field.getText());
        }

        public void restoreState() {
            field.setText(oldValue);
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
        return ExecUtil.readCommandLine(null, cmd, versionFlag);
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
