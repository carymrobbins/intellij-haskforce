package com.haskforce.settings;

import com.haskforce.ui.JTextAccessorField;
import com.haskforce.utils.ExecUtil;
import com.haskforce.utils.GuiUtil;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.ui.JBColor;
import com.intellij.ui.RawCommandLineEditor;
import com.intellij.ui.TextAccessor;
import com.intellij.util.messages.Topic;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;
import java.awt.*;
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
    private TextFieldWithBrowseButton ghcModiPath;
    private JButton ghcModiAutoFind;
    private JTextField ghcModiVersion;
    private RawCommandLineEditor ghcModiFlags;
    private JTextAccessorField ghcModiTimeout;

    private List<Property> properties;

    public HaskellToolsConfigurable(@NotNull Project project) {
        this.propertiesComponent = PropertiesComponent.getInstance(project);
        properties = Arrays.asList(
                new Tool(project, "stylish-haskell", ToolKey.STYLISH_HASKELL_KEY, stylishPath, stylishFlags,
                        stylishAutoFind, stylishVersion),
                new Tool(project, "hlint", ToolKey.HLINT_KEY, hlintPath, hlintFlags,
                         hlintAutoFind, hlintVersion),
                new Tool(project, "ghc-mod", ToolKey.GHC_MOD_KEY, ghcModPath, ghcModFlags,
                         ghcModAutoFind, ghcModVersion, "version"),
                new Tool(project, "ghc-modi", ToolKey.GHC_MODI_KEY, ghcModiPath, ghcModiFlags,
                         ghcModiAutoFind, ghcModiVersion, "version", SettingsChangeNotifier.GHC_MODI_TOPIC),
                new PropertyField(ToolKey.GHC_MODI_TIMEOUT_KEY, ghcModiTimeout, Long.toString(ToolKey.getGhcModiTimeout(project)))
        );
        // Validate that we can only enter numbers in the timeout field.
        final Color originalBackground = ghcModiTimeout.getBackground();
        ghcModiTimeout.setInputVerifier(new InputVerifier() {
            @Override
            public boolean verify(JComponent input) {
                JTextAccessorField field = (JTextAccessorField)input;
                try {
                    //noinspection ResultOfMethodCallIgnored
                    Long.parseLong(field.getText());
                } catch (NumberFormatException e) {
                    field.setBackground(JBColor.RED);
                    return false;
                }
                field.setBackground(originalBackground);
                return true;
            }
        });
    }

    interface Property {
        public boolean isModified();
        public void saveState();
        public void restoreState();
    }

    interface Versioned {
        public void updateVersion();
    }

    class PropertyField implements Property {
        public String oldValue;
        public String propertyKey;
        public final TextAccessor field;

        PropertyField(@NotNull String propertyKey, @NotNull TextAccessor field) {
            this(propertyKey, field, "");
        }

        PropertyField(@NotNull String propertyKey, @NotNull TextAccessor field, @NotNull String defaultValue) {
            this.propertyKey = propertyKey;
            this.field = field;
            this.oldValue = propertiesComponent.getValue(propertyKey, defaultValue);
            field.setText(oldValue);
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

    class Tool implements Property, Versioned {
        public final Project project;
        public final String command;
        public final ToolKey key;
        public final TextFieldWithBrowseButton pathField;
        public final RawCommandLineEditor flagsField;
        public final JTextField versionField;
        public final String versionParam;
        public final JButton autoFindButton;
        public final List<PropertyField> propertyFields;
        public final @Nullable Topic<SettingsChangeNotifier> topic;
        private final @Nullable SettingsChangeNotifier publisher;

        Tool(Project project, String command, ToolKey key, TextFieldWithBrowseButton pathField,
             RawCommandLineEditor flagsField, JButton autoFindButton, JTextField versionField) {
            this(project, command, key, pathField, flagsField, autoFindButton, versionField, "--version");
        }

        Tool(Project project, String command, ToolKey key, TextFieldWithBrowseButton pathField,
             RawCommandLineEditor flagsField, JButton autoFindButton, JTextField versionField, String versionParam) {
            this(project, command, key, pathField, flagsField, autoFindButton, versionField, versionParam, null);
        }

        Tool(Project project, String command, ToolKey key, TextFieldWithBrowseButton pathField,
             RawCommandLineEditor flagsField, JButton autoFindButton, JTextField versionField, String versionParam,
             @Nullable Topic<SettingsChangeNotifier> topic) {
            this.project = project;
            this.command = command;
            this.key = key;
            this.pathField = pathField;
            this.flagsField = flagsField;
            this.versionField = versionField;
            this.versionParam = versionParam;
            this.autoFindButton = autoFindButton;
            this.topic = topic;
            this.publisher = topic == null ? null : project.getMessageBus().syncPublisher(topic);

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

        public void saveState() {
            if (isModified() && publisher != null) {
                publisher.onSettingsChanged(new ToolSettings(pathField.getText(), flagsField.getText()));
            }
            for (PropertyField propertyField : propertyFields) {
                propertyField.saveState();
            }
        }

        public void restoreState() {
            for (PropertyField propertyField : propertyFields) {
                propertyField.restoreState();
            }
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
        for (Property property : properties) {
            if (property.isModified()) {
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
        for (Property property : properties) {
            if (property instanceof Versioned) {
                ((Versioned)property).updateVersion();
            }
        }
    }

    /**
     * Persistent save of the current state.
     */
    private void saveState() {
        for (Property property : properties) {
            property.saveState();
        }
    }

    /**
     * Restore components to the initial state.
     */
    private void restoreState() {
        for (Property property : properties) {
            property.restoreState();
        }
    }
}
