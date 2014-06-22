package com.haskforce.run;

import com.intellij.execution.configuration.AbstractRunConfiguration;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.openapi.project.Project;

public abstract class HaskellRunConfigurationBase extends AbstractRunConfiguration {
    public HaskellRunConfigurationBase(Project project, ConfigurationFactory factory) {
        super(project, factory);
    }
}
