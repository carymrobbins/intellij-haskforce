package com.haskforce.run;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.CommandLineState;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.OSProcessHandler;
import com.intellij.execution.process.ProcessHandler;
import com.intellij.execution.runners.ExecutionEnvironment;
import org.jetbrains.annotations.NotNull;

public class HaskellCommandLineState extends CommandLineState {
    protected HaskellCommandLineState(ExecutionEnvironment environment) {
        super(environment);
    }

    @NotNull
    @Override
    protected OSProcessHandler startProcess() throws ExecutionException {
        GeneralCommandLine commandLine = new GeneralCommandLine();
        commandLine.setWorkDirectory(getEnvironment().getProject().getBasePath());
        // TODO: This should probably be a bit more generic than relying on `cabal run`.
        commandLine.setExePath("cabal");
        commandLine.addParameter("run");
        // TODO: Apply user arguments from run config.
        return new OSProcessHandler(commandLine.createProcess());
    }
}
