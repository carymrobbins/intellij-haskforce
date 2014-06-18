package com.haskforce.run;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.ParametersList;
import com.intellij.execution.configurations.RunProfile;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import com.sun.tools.javac.util.List;
import org.jetbrains.annotations.NotNull;

import java.util.ArrayList;

public class HaskellCommandLineState extends CommandLineState {

    private final HaskellRunConfiguration myConfig;

    protected HaskellCommandLineState(ExecutionEnvironment environment, HaskellRunConfiguration runConfiguration) {
        super(environment);
        myConfig = runConfiguration;
    }

    @NotNull
    @Override
    protected OSProcessHandler startProcess() throws ExecutionException {
        ExecutionEnvironment env = getEnvironment();
        GeneralCommandLine commandLine = new GeneralCommandLine();
        commandLine.setWorkDirectory(env.getProject().getBasePath());
        // TODO: This should probably be a bit more generic than relying on `cabal run`.
        commandLine.setExePath("cabal");
        ParametersList parametersList = commandLine.getParametersList();
        parametersList.add("run");
        parametersList.addParametersString(myConfig.programArguments);
        return new OSProcessHandler(commandLine.createProcess());
    }
}
