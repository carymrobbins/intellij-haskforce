package com.haskforce.settings.experimental;

import com.intellij.openapi.options.SearchableConfigurable;

import javax.swing.*;

public abstract class AbstractHaskForceExperimentalSettingsConfigurable implements SearchableConfigurable {
  JPanel mainPanel;
  JCheckBox ghcPkgEnabled;
  JTextField ghcPkgTimeoutMillis;
}
