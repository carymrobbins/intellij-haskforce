package com.haskforce.run;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.*;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

public class HaskellRunConfiguration extends ModuleBasedConfiguration<RunConfigurationModule> {
    public String programArguments;

    public HaskellRunConfiguration(@NotNull String name, @NotNull Project project, HaskellRunConfigurationType configurationType) {
        super(name, new RunConfigurationModule(project), configurationType.getConfigurationFactories()[0]);
    }

    @Override
    public Collection<Module> getValidModules() {
        return null;
    }

    @NotNull
    @Override
    public HaskellRunConfigurationEditorForm getConfigurationEditor() {
        return new HaskellRunConfigurationEditorForm();
    }

    @Nullable
    @Override
    public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment executionEnvironment) throws ExecutionException {
        // TODO: return new HaskellCommandLineState(...)
        return null;
    }
}
