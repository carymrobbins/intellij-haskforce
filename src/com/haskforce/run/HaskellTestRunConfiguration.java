package com.haskforce.run;

import com.haskforce.settings.HaskellBuildSettings;
import com.haskforce.settings.HaskellCompilerConfigurable;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.Executor;
import com.intellij.execution.configurations.ConfigurationFactory;
import com.intellij.execution.configurations.RunProfileState;
import com.intellij.execution.configurations.RuntimeConfigurationException;
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
 * As demonstrated, use a unique string pathKey for each configuration variable and implement its read
 * and write.
 */
public class HaskellTestRunConfiguration extends HaskellRunConfigurationBase {

    // Element keys for readExternal and writeExternal to save configuration.
    public static final String PROGRAM_ARGUMENTS = "PROGRAM_ARGUMENTS";

    // Local configuration variables.
    public String programArguments;

    protected HaskellTestRunConfiguration(@NotNull Project project, @NotNull ConfigurationFactory factory) {
        super(project, factory);
    }

    @NotNull
    @Override
    public HaskellTestRunConfigurationEditorForm getConfigurationEditor() {
        return new HaskellTestRunConfigurationEditorForm();
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
        // TODO: How do we integrate with the test output UI?

        // Running the tests should automatically enable tests in the build settings.
        HaskellBuildSettings.getInstance(getProject()).setEnableTests(true);
        return new HaskellTestCommandLineState(executionEnvironment, this);
    }

    @Override
    public Collection<Module> getValidModules() {
        // TODO: Use main modules at first, but later - how do we differentiate main modules from test modules?
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
