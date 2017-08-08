package com.haskforce.eta.run;

import javax.swing.*;

public class EtlasRunConfigurationForm {
    JPanel mainPanel;
    com.intellij.ui.RawCommandLineEditor programArguments;
    com.intellij.openapi.ui.TextFieldWithBrowseButton workingDirectory;
    com.intellij.ui.RawCommandLineEditor vmArguments;
    com.intellij.execution.configuration.EnvironmentVariablesComponent environmentVariables;
}
