package com.haskforce.run;

import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.ConfigurationType;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

public class HaskellRunConfigurationFactory extends ConfigurationFactory {
    protected HaskellRunConfigurationFactory(@NotNull ConfigurationType type) {
        super(type);
    }

    @Override
    public RunConfiguration createTemplateConfiguration(@NotNull Project project) {
        return new HaskellRunConfiguration("Haskell Application", project, HaskellRunConfigurationType.getInstance());
    }
}
