package com.haskforce.run;

import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.ConfigurationType;
import com.intellij.execution.configurations.RunConfiguration;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;

/**
 * Creates the run configurations.  If we have multiple ConfigurationFactories they will appear as sub-options
 * to Haskell under the run configuration types - Run->Edit Configurations->[+]->Haskell
 */
public class HaskellApplicationConfigurationFactory extends ConfigurationFactory {
    protected HaskellApplicationConfigurationFactory(@NotNull ConfigurationType type) {
        super(type);
    }

    @Override
    public RunConfiguration createTemplateConfiguration(@NotNull Project project) {
        return new HaskellApplicationRunConfiguration(project, this);
    }
}
