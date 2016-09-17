package com.haskforce.haskell.run;

import com.haskforce.settings.HaskellBuildSettings;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.ParametersList;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import org.jetbrains.annotations.NotNull;

public class HaskellTestCommandLineState extends CommandLineState {

    private final HaskellTestRunConfiguration myConfig;

    protected HaskellTestCommandLineState(ExecutionEnvironment environment, HaskellTestRunConfiguration runConfiguration) {
        super(environment);
        myConfig = runConfiguration;
    }

    @NotNull
    @Override
    protected OSProcessHandler startProcess() throws ExecutionException {
        ExecutionEnvironment env = getEnvironment();
        GeneralCommandLine commandLine = new GeneralCommandLine();
        commandLine.setWorkDirectory(env.getProject().getBasePath());
        // TODO: This should probably be a bit more generic than relying on `cabal test`.
        final String cabalPath = HaskellBuildSettings.getInstance(myConfig.getProject()).getCabalPath();
        commandLine.setExePath(cabalPath);
        ParametersList parametersList = commandLine.getParametersList();
        parametersList.add("test");
        parametersList.addParametersString(myConfig.programArguments);
        return new OSProcessHandler(commandLine);
    }
}
