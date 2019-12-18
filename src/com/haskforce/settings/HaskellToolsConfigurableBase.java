package com.haskforce.settings;

import com.haskforce.ui.JTextAccessorField;
import com.intellij.openapi.options.SearchableConfigurable;
import com.intellij.openapi.ui.TextFieldWithBrowseButton;
import com.intellij.ui.RawCommandLineEditor;

import javax.swing.*;

/**
 * The "Haskell Tools" option in Preferences->Project Settings.
 * Java bits exist here for easy GUI form builder stuff.
 * Real implementation exists at {@link HaskellToolsConfigurable}.
 */
public abstract class HaskellToolsConfigurableBase implements SearchableConfigurable {
    JPanel mainPanel;
    TextFieldWithBrowseButton stylishPath;
    RawCommandLineEditor stylishFlags;
    JButton stylishAutoFind;
    JTextField stylishVersion;
    TextFieldWithBrowseButton hlintPath;
    RawCommandLineEditor hlintFlags;
    JButton hlintAutoFind;
    JTextField hlintVersion;

    JCheckBox hsdevEnabled;
    TextFieldWithBrowseButton hsdevPath;
    RawCommandLineEditor hsdevFlags;
    JButton hsdevAutoFind;
    JTextField hsdevVersion;
    JTextAccessorField hsdevPort;
    JCheckBox hsdevSpawnServer;
    RawCommandLineEditor hsdevScanFlags;
    JTextAccessorField hsdevScanTimeout;
    JTextAccessorField hsdevCommandTimeout;

    TextFieldWithBrowseButton ghcModPath;
    RawCommandLineEditor ghcModFlags;
    JButton ghcModAutoFind;
    JTextField ghcModVersion;
    TextFieldWithBrowseButton ghcModiPath;
    JButton ghcModiAutoFind;
    JTextField ghcModiVersion;
    RawCommandLineEditor ghcModiFlags;
    JTextAccessorField ghcModiResponseTimeout;
    JTextAccessorField ghcModiKillIdleTimeout;
    TextFieldWithBrowseButton hindentPath;
    JButton hindentAutoFind;
    JTextField hindentVersion;
    RawCommandLineEditor hindentFlags;
}
