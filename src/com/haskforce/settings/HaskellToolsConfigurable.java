package com.haskforce.settings;

import com.haskforce.ui.JTextAccessorField;
import com.haskforce.utils.ExecUtil;
import com.haskforce.utils.GuiUtil;
import com.haskforce.utils.NotificationUtil;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.options.ConfigurationException;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.openapi.util.Pair;
import com.intellij.ui.JBColor;
import com.intellij.ui.RawCommandLineEditor;
import com.intellij.ui.TextAccessor;
import com.intellij.util.messages.Topic;
import org.jetbrains.annotations.Nls;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.Option;
import scala.runtime.AbstractFunction1;

import javax.swing.*;
import java.awt.*;
import java.io.File;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The "Haskell Tools" option in Preferences->Project Settings.
 */
public class HaskellToolsConfigurable implements SearchableConfigurable {
    public static final String HASKELL_TOOLS_ID = "Haskell Tools";
    private static final Logger LOG = Logger.getInstance(HaskellToolsConfigurable.class);

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

    private TextFieldWithBrowseButton hsdevPath;
    private RawCommandLineEditor hsdevFlags;
    private JButton hsdevAutoFind;
    private JTextField hsdevVersion;
    private JTextAccessorField hsdevPort;
    private JCheckBox hsdevSpawnServer;

    private TextFieldWithBrowseButton ghcModPath;
    private RawCommandLineEditor ghcModFlags;
    private JButton ghcModAutoFind;
    private JTextField ghcModVersion;
    private TextFieldWithBrowseButton ghcModiPath;
    private JButton ghcModiAutoFind;
    private JTextField ghcModiVersion;
    private RawCommandLineEditor ghcModiFlags;
    private JTextAccessorField ghcModiResponseTimeout;
    private JTextAccessorField ghcModiKillIdleTimeout;
    private TextFieldWithBrowseButton hindentPath;
    private JButton hindentAutoFind;
    private JTextField hindentVersion;
    private RawCommandLineEditor hindentFlags;

    private List<Property> properties;

    public HaskellToolsConfigurable(@NotNull Project project) {
        this.propertiesComponent = PropertiesComponent.getInstance(project);
        properties = Arrays.asList(
                new Tool(project, "stylish-haskell", ToolKey.STYLISH_HASKELL_KEY, stylishPath, stylishFlags,
                        stylishAutoFind, stylishVersion, "--help"),
                new Tool(project, "hlint", ToolKey.HLINT_KEY, hlintPath, hlintFlags,
                         hlintAutoFind, hlintVersion),

                new HsDevTool(project),
                // TODO: These need to somehow be with HsDevTool
                new PropertyField(ToolKey.HSDEV_PORT_KEY, hsdevPort),
                new PropertyCheckBox(ToolKey.HSDEV_SPAWN_SERVER_KEY, hsdevSpawnServer, ToolKey.getHsDevSpawnServer(project)),

                new Tool(project, "ghc-mod", ToolKey.GHC_MOD_KEY, ghcModPath, ghcModFlags,
                         ghcModAutoFind, ghcModVersion, "version"),
                new Tool(project, "ghc-modi", ToolKey.GHC_MODI_KEY, ghcModiPath, ghcModiFlags,
                         ghcModiAutoFind, ghcModiVersion, "version", SettingsChangeNotifier.GHC_MODI_TOPIC),
                new PropertyField(ToolKey.GHC_MODI_RESPONSE_TIMEOUT_KEY, ghcModiResponseTimeout, Long.toString(ToolKey.getGhcModiResponseTimeout(project))),
                new PropertyField(ToolKey.GHC_MODI_KILL_IDLE_TIMEOUT_KEY, ghcModiKillIdleTimeout, Long.toString(ToolKey.getGhcModiKillIdleTimeout(project))),
                new Tool(project, "hindent", ToolKey.HINDENT_KEY, hindentPath, hindentFlags,
                    hindentAutoFind, hindentVersion)
        );

        setNumericInputVerifier(ghcModiResponseTimeout);
        setNumericInputVerifier(ghcModiKillIdleTimeout);
    }

    private void setNumericInputVerifier(JComponent component) {
        final Color originalBackground = component.getBackground();
        component.setInputVerifier(new InputVerifier() {
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
        boolean isModified();
        void saveState();
        void restoreState();
    }

    interface Versioned {
        void updateVersion();
    }

    class PropertyCheckBox implements Property {

        public boolean oldValue;
        public final String propertyKey;
        public final JCheckBox field;

        PropertyCheckBox(@NotNull String propertyKey, @NotNull JCheckBox field, boolean defaultValue) {
          this.propertyKey = propertyKey;
          this.field = field;
          this.oldValue = defaultValue;
        }

        @Override
        public boolean isModified() {
            return field.isSelected() == oldValue;
        }

        @Override
        public void saveState() {
          propertiesComponent.setValue(propertyKey, oldValue = field.isSelected());
        }

        @Override
        public void restoreState() {
          field.setSelected(oldValue);
        }
    }

    /**
     * Manages the state of a PropertyComponent and its respective field.
     */
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

    /**
     * Manages the group of fields which reside to a particular tool.
     */
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
                // Get the first line reported from `getVersion`
                String v = getVersion(pathText, versionParam);
                if (v == null) return;
                String[] lines = NEWLINE_REGEX.split(v);
                if (lines.length == 0) return;
                versionField.setText(lines[0]);
            }
        }

        private Pattern NEWLINE_REGEX = Pattern.compile("\r\n|\r|\n");

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

    class HsDevTool extends Tool {
        HsDevTool(Project project) {
            super(
              project,
              "hsdev",
              ToolKey.HSDEV_KEY,
              hsdevPath,
              hsdevFlags,
              hsdevAutoFind,
              hsdevVersion
            );
        }

        @Override
        public void updateVersion() {
            String pathText = pathField.getText();
            if (pathText.isEmpty()) {
                versionField.setText("");
                return;
            }
            versionField.setText(getHsDevVersion(pathText));
        }

        // Returns null on error which will clear the versionField when
        // passed to versionField.set()
        @Nullable
        private String getHsDevVersion(String hsdevPath) {
            return ExecUtil.readCommandLine(
                new GeneralCommandLine(hsdevPath, "version", "--compiler")
            ).fold(
              e -> {
                  NotificationUtil.displaySimpleNotification(
                    NotificationType.ERROR, null, "Haskell Tools", e.getMessage()
                  );
                  return null;
              },
              s -> s.replace('\r', ' ').replace('\n', ' ')
            );
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
        validate();
        updateVersionInfoFields();
        saveState();
    }

    public void validate() throws ConfigurationException {
        validateExecutableIfNonEmpty("stylish", stylishPath);
        validateExecutableIfNonEmpty("hlint", hlintPath);
        // Validate ghcModPath if either it or ghcModiPath have been set.
        if (ghcModPath.getText().isEmpty() && !ghcModiPath.getText().isEmpty()) {
            throw new ConfigurationException("ghc-mod must be configured if ghc-modi is configured.");
        }
        validateExecutableIfNonEmpty("ghc-mod", ghcModPath);
        validateExecutableIfNonEmpty("ghc-modi", ghcModiPath);
        validateExecutableIfNonEmpty("hindent", hindentPath);
    }

    public void validateExecutable(String name, TextAccessor field) throws ConfigurationException {
        if (new File(field.getText()).canExecute()) return;
        throw new ConfigurationException("Not a valid '" + name + "' executable: '" + field.getText() + "'");
    }

    public void validateExecutableIfNonEmpty(String name, TextAccessor field) throws ConfigurationException {
        if (field.getText().isEmpty()) return;
        validateExecutable(name, field);
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
    @Nullable
    private static String getVersion(String cmd, String versionFlag) {
        return ExecUtil.readCommandLine(null, cmd, versionFlag).fold(
            new AbstractFunction1<ExecUtil.ExecError, String>() {
                @Override
                public String apply(ExecUtil.ExecError e) {
                    NotificationUtil.displaySimpleNotification(
                        NotificationType.ERROR, null, "Haskell Tools", e.getMessage()
                    );
                    return null;
                }
            },
            new AbstractFunction1<String, String>() {
                @Override
                public String apply(String s) {
                    return s;
                }
            }
        );
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
        preSaveHook();
        for (Property property : properties) {
            property.saveState();
        }
    }

    /**
     * Updates tool settings before saving.
     */
    private void preSaveHook() {
        ghcModLegacyInteractivePreSaveHook();
    }

    private static Pattern GHC_MOD_VERSION_REGEX = Pattern.compile("(\\d+)\\.(\\d+)");

    @Nullable
    public static Pair<Integer, Integer> parseGhcModVersion(String version) {
        if (version == null) return null;
        Matcher m = GHC_MOD_VERSION_REGEX.matcher(version);
        if (!m.find()) {
            LOG.error("Could not find ghc-mod version number from string: " + version);
            return null;
        }
        return Pair.create(Integer.parseInt(m.group(1)), Integer.parseInt(m.group(2)));
    }

    /**
     * If we're using ghc-mod >= 5.4, ghc-modi will be configured as `ghc-mod legacy-interactive`
     */
    private void ghcModLegacyInteractivePreSaveHook() {
        // If ghc-mod is not configured or is not >= 5.4, we can't infer legacy-interactive.
        if (ghcModPath.getText().isEmpty() || !isGhcMod5_4(ghcModPath.getText())) return;
        // If ghc-modi is configured and it is not >= 5.4, leave it alone.
        if (!ghcModiPath.getText().isEmpty() && !isGhcMod5_4(ghcModiPath.getText())) return;
        // If all is good, configure ghc-modi as legacy-interactive.
        ghcModiPath.setText(ghcModPath.getText());
        // If the current ghc-modi flags contains the `legacy-interactive` command, do not add it back.
        if (!ghcModiFlags.getText().contains("legacy-interactive")) {
            ghcModiFlags.setText(ghcModiFlags.getText() + " legacy-interactive");
        }
    }

    private boolean isGhcMod5_4(String exePath) {
        String versionStr = getVersion(exePath, "version");
        if (versionStr == null) {
            LOG.warn("Could not retrieve ghc-mod version from " + exePath);
            return false;
        }
        Pair<Integer, Integer> version = parseGhcModVersion(versionStr);
        if (version == null) {
            LOG.warn("Could not parse ghc-mod version from string: " + versionStr);
            return false;
        }
        return version.first > 5 || (version.first == 5 && version.second >= 4);
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
