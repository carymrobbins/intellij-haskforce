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

    private Process runCommand(String command, String arg) throws IOException {
        ProcessWrapper p = new ProcessWrapper(myCabalFile.getParentFile().getCanonicalPath());
        return arg == null ? p.getProcess(myCabalPath, command)
                           : p.getProcess(myCabalPath, command, arg);
    }

    public Process configure() throws IOException {
        return runCommand("configure", myCabalFlags);
    }

    public Process build() throws IOException {
        return runCommand("build", null);
    }

    public Process clean() throws IOException {
        return runCommand("clean", null);
    }
}
