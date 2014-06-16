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

/**
 * Manages the data and execution of run configurations - Run->Edit Configurations->[+]->Haskell
 */
public class HaskellRunConfiguration extends RunConfigurationBase {
    public String programArguments;

    protected HaskellRunConfiguration(@NotNull Project project, @NotNull ConfigurationFactory factory, @NotNull String name) {
        super(project, factory, name);
    }

    @NotNull
    @Override
    public HaskellRunConfigurationEditorForm getConfigurationEditor() {
        return new HaskellRunConfigurationEditorForm();
    }

    @Override
    public void checkConfiguration() throws RuntimeConfigurationException {
       // TODO
    }

    @Nullable
    @Override
    public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment executionEnvironment) throws ExecutionException {
        // TODO: return new HaskellCommandLineState(...)
        return null;
    }
}
