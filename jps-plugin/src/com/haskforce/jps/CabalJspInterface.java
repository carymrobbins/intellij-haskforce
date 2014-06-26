package com.haskforce.jps;

/*
 * Downloaded from https://github.com/Atsky/haskell-idea-plugin on 7 May
 * 2014.
 */

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
    private String myCabalPath;
    @NotNull
    private String myCabalFlags;

    CabalJspInterface(@NotNull String cabalPath, @NotNull String cabalFlags,
                      @NotNull File cabalFile) {
        myCabalPath = cabalPath;
        myCabalFile = cabalFile;
        myCabalFlags = cabalFlags;
    }

    private Process runCommand(String command, String... args) throws IOException {
        final int numPrefixArgs = 2;
        String[] fullCommand = new String[args.length + numPrefixArgs];
        fullCommand[0] = myCabalPath;
        fullCommand[1] = command;
        System.arraycopy(args, 0, fullCommand, numPrefixArgs, args.length);
        ProcessWrapper p = new ProcessWrapper(myCabalFile.getParentFile().getCanonicalPath());
        return p.getProcess(fullCommand);
    }

    public Process sandboxInit() throws IOException {
        return runCommand("sandbox", "init");
    }

    public Process installDependencies() throws IOException {
        return runCommand("install", "--only-dependencies");
    }

    public Process configure() throws IOException {
        return runCommand("configure", myCabalFlags);
    }

    public Process build() throws IOException {
        return runCommand("build");
    }

    public Process clean() throws IOException {
        return runCommand("clean");
    }
}
