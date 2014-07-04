package com.haskforce.jps;

/*
 * Downloaded from https://github.com/Atsky/haskell-idea-plugin on 7 May
 * 2014.
 */

import com.haskforce.jps.model.HaskellBuildOptions;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.ParametersList;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.io.IOException;

/**
 * Simple cabal interface.
 */
public class CabalJspInterface {
    @NotNull
    private File myCabalFile;

    @NotNull
    private HaskellBuildOptions myBuildOptions;

    CabalJspInterface(@NotNull File cabalFile, @NotNull HaskellBuildOptions buildOptions) {
        myCabalFile = cabalFile;
        myBuildOptions = buildOptions;
    }

    private Process runCommand(String command, String... args) throws IOException, ExecutionException {
        return getCommandLine(command, args).createProcess();
    }

    private GeneralCommandLine getCommandLine(String command, String... args) throws IOException, ExecutionException {
        GeneralCommandLine commandLine = new GeneralCommandLine();
        commandLine.setWorkDirectory(myCabalFile.getParentFile().getCanonicalPath());
        commandLine.setExePath(myBuildOptions.myCabalPath);
        ParametersList parametersList = commandLine.getParametersList();
        parametersList.add(command);
        if (command.equals("install") || command.equals("configure")) {
            if (myBuildOptions.myEnableTests) {
                parametersList.add("--enable-tests");
            }
            if (!myBuildOptions.myProfilingBuild) {
                parametersList.add("--disable-library-profiling");
            }
        }
        parametersList.addAll(args);
        commandLine.setRedirectErrorStream(true);
        return commandLine;
    }

    public Process sandboxInit() throws IOException, ExecutionException {
        return runCommand("sandbox", "init");
    }

    public Process installDependencies(boolean dryRun) throws IOException, ExecutionException {
        return runCommand("install", "--only-dependencies", dryRun ? "--dry-run" : "");
    }

    public Process installDependencies() throws IOException, ExecutionException {
        return installDependencies(false);
    }

    public Process configure() throws IOException, ExecutionException {
        GeneralCommandLine commandLine = getCommandLine("configure");
        ParametersList parametersList = commandLine.getParametersList();
        parametersList.addParametersString(myBuildOptions.myCabalFlags);
        return commandLine.createProcess();
    }

    public Process build() throws IOException, ExecutionException {
        return runCommand("build");
    }

    public Process clean() throws IOException, ExecutionException {
        return runCommand("clean");
    }
}
