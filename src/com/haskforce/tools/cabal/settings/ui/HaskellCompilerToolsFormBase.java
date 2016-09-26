package com.haskforce.tools.cabal.settings.ui;

import com.intellij.openapi.ui.TextFieldWithBrowseButton;

import javax.swing.*;

/**
 * Base form for configuring tools at project creation.  As a Java form, it binds easily with
 * IntelliJ's form designer.  Extend functionality using the HaskellCompilerToolsForm Scala class.
 */
public abstract class HaskellCompilerToolsFormBase {
    protected TextFieldWithBrowseButton cabalPath;
    protected JCheckBox cabalSetAsDefault;
    protected TextFieldWithBrowseButton ghcPath;
    protected TextFieldWithBrowseButton ghcModPath;
    protected TextFieldWithBrowseButton ghcModiPath;
    protected JCheckBox ghcSetAsDefault;
    protected JCheckBox ghcModSetAsDefault;
    protected JCheckBox ghcModiSetAsDefault;
    protected JPanel contentPane;
}
