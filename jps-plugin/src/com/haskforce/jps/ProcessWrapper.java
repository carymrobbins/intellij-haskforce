package com.haskforce.jps;

/*
 * Downloaded from https://github.com/Atsky/haskell-idea-plugin on 7 May
 * 2014.
 */

import java.io.File;
import java.io.IOException;
import java.util.Arrays;

/**
 * Execution helper that provides a process.
 */
public class ProcessWrapper {
    private String myWorkingDirectory;

    ProcessWrapper(String workingDirectory) {
        myWorkingDirectory = workingDirectory;
    }

    public Process getProcess(String ... cmd) throws IOException {
        ProcessBuilder processBuilder = new ProcessBuilder(Arrays.asList(cmd));
        if (myWorkingDirectory != null) {
            processBuilder.directory(new File(myWorkingDirectory));
        }

        processBuilder.redirectErrorStream(true);
        return processBuilder.start();
    }
}
