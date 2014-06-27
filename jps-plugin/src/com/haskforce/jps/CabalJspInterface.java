package com.haskforce.jps;

/*
 * Downloaded from https://github.com/Atsky/haskell-idea-plugin on 7 May
 * 2014.
 */

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
    private String myCabalFlags;
    @NotNull
    private Boolean myEnableTests;
    @NotNull
    private String myCabalPath;

    CabalJspInterface(@NotNull String cabalPath, @NotNull String cabalFlags,
                      @NotNull Boolean enableTests, @NotNull File cabalFile) {
        myCabalPath = cabalPath;
        myCabalFlags = cabalFlags;
        myEnableTests = enableTests;
        myCabalFile = cabalFile;
    }

    private Process runCommand(String command, String... args) throws IOException, ExecutionException {
        return getCommandLine(command, args).createProcess();
    }

    private GeneralCommandLine getCommandLine(String command, String... args) throws IOException, ExecutionException {
        GeneralCommandLine commandLine = new GeneralCommandLine();
        commandLine.setWorkDirectory(myCabalFile.getParentFile().getCanonicalPath());
        commandLine.setExePath(myCabalPath);
        ParametersList parametersList = commandLine.getParametersList();
        parametersList.add(command);
        if (myEnableTests && (command.equals("install") || command.equals("configure"))) {
            parametersList.add("--enable-tests");
        }
        parametersList.addAll(args);
        commandLine.setRedirectErrorStream(true);
        return commandLine;
    }

    public Process sandboxInit() throws IOException, ExecutionException {
        return runCommand("sandbox", "init");
    }

    public Process installDependencies() throws IOException, ExecutionException {
        return runCommand("install", "--only-dependencies");
    }

    public Process configure() throws IOException, ExecutionException {
        GeneralCommandLine commandLine = getCommandLine("configure");
        ParametersList parametersList = commandLine.getParametersList();
        parametersList.addParametersString(myCabalFlags);
        return commandLine.createProcess();
    }

    public Process build() throws IOException, ExecutionException {
        return runCommand("build");
    }

    public Process clean() throws IOException, ExecutionException {
        return runCommand("clean");
    }
}
