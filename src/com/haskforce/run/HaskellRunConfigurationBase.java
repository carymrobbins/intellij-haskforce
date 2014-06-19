package com.haskforce.run;

import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.RunConfigurationBase;
import com.intellij.openapi.project.Project;

public abstract class HaskellRunConfigurationBase extends RunConfigurationBase {
    protected HaskellRunConfigurationBase(Project project, ConfigurationFactory factory, String name) {
        super(project, factory, name);
    }
}
