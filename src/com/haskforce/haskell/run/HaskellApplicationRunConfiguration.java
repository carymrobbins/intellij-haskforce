package com.haskforce.haskell.run;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.*;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.intellij.openapi.components.PathMacroManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.InvalidDataException;
import com.intellij.openapi.util.JDOMExternalizerUtil;
import com.intellij.openapi.util.WriteExternalException;
import org.jdom.Element;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;

/**
 * Manages the data and execution of run configurations - Run->Edit Configurations->[+]->Haskell
 *
 * Each configuration variable must be read and written from the readExternal and writeExternal
 * methods, respectively.  Otherwise, user configuration is not persisted across IntelliJ restarts.
 * As demonstrated, use a unique string key for each configuration variable and implement its read
 * and write.
 */
public class HaskellApplicationRunConfiguration extends HaskellRunConfigurationBase {

    // Element keys for readExternal and writeExternal to save configuration.
    public static final String PROGRAM_ARGUMENTS = "PROGRAM_ARGUMENTS";

    // Local configuration variables.
    public String programArguments;

    protected HaskellApplicationRunConfiguration(@NotNull Project project, @NotNull ConfigurationFactory factory) {
        super(project, factory);
    }

    @NotNull
    @Override
    public HaskellApplicationRunConfigurationEditorForm getConfigurationEditor() {
        return new HaskellApplicationRunConfigurationEditorForm();
    }

    /**
     * If the user is not using cabal 1.18 or higher, an error is displayed in the run configuration dialog.
     */
    @Override
    public void checkConfiguration() throws RuntimeConfigurationException {
        requireCabal1_18();
    }

    @Nullable
    @Override
    public RunProfileState getState(@NotNull Executor executor, @NotNull ExecutionEnvironment executionEnvironment) throws ExecutionException {
        return new HaskellApplicationCommandLineState(executionEnvironment, this);
    }

    @Override
    public Collection<Module> getValidModules() {
        // TODO: Lookup modules with main defined.
        return null;
    }

    @Override
    public void readExternal(Element element) throws InvalidDataException {
        PathMacroManager.getInstance(getProject()).expandPaths(element);
        super.readExternal(element);
        programArguments = JDOMExternalizerUtil.readField(element, PROGRAM_ARGUMENTS);
    }

    @Override
    public void writeExternal(Element element) throws WriteExternalException {
        super.writeExternal(element);
        JDOMExternalizerUtil.writeField(element, PROGRAM_ARGUMENTS, programArguments);
        PathMacroManager.getInstance(getProject()).collapsePathsRecursively(element);
    }
}
