package com.haskforce.utils;

import com.haskforce.HaskellSdkType;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.util.SystemInfo;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.nio.charset.Charset;
import java.util.List;

/**
 * Helper class to perform execution related tasks, including locating programs.
 */
public class ExecUtil {
    // Messages go to the log available in Help -> Show log in finder.
    private final static Logger LOG = Logger.getInstance(HaskellSdkType.class);

    /**
     * Execute a command using the default shell.
     */
    @Nullable
    public static String exec(@NotNull final String command) {
        // Find some valid working directory, doesn't matter which one.
        ProjectManager pm = ProjectManager.getInstance();
        Project[] projects = pm == null ? null : pm.getOpenProjects();
        String workDir = ".";
        // Set the working directory if there is an open project.
        if (pm == null || projects.length == 0) {
            LOG.info("No open projects so cannot find a valid path. Using '.'.");
        } else {
            workDir = projects[0].getBaseDir().getCanonicalPath();
        }
        return exec(workDir, command);
    }

    /**
     * Execute a command using the default shell in a given work directory.
     */
    @Nullable
    public static String exec(@NotNull final String workDir, @NotNull final String command) {
        // Setup shell and the GeneralCommandLine.
        //
        // Getting the right PATH among other things is apparently tricky,
        // but this works regardless if I start IntelliJ through Spotlight or
        // a terminal.
        //
        // WARNING: Running the plugin in IntelliJ directly gives a different
        //          environment. Install the plugin and make sure that adding
        //          an Haskell SDK still autodetects things right.
        final GeneralCommandLine commandLine = new GeneralCommandLine();
        commandLine.setWorkDirectory(workDir);
        commandLine.setPassParentEnvironment(true);
        if (SystemInfo.isWindows) {
            commandLine.setExePath("cmd");
            commandLine.addParameter("/c");
        } else {
            // Default to UNIX if not Windows.
            commandLine.setExePath("/bin/sh");
            commandLine.addParameter("-c");
        }
        commandLine.addParameter(command);

        ProcessOutput output = null;
        try {
            output = new CapturingProcessHandler(commandLine.createProcess(),
                            Charset.defaultCharset(), commandLine.getCommandLineString()).runProcess();
        } catch (ExecutionException e) {
            LOG.info("Failed executing " + command);
            LOG.info("Message: " + e.getMessage());
            return null;
        }

        if (output == null) {
            LOG.info("No output from " + command);
            return null;
        }

        List<String> lines = output.getStdoutLines();
        StringBuilder sb = new StringBuilder(100*lines.size());
        for (String line : lines) {
            sb.append(line);
        }
        return sb.toString();
    }

    /**
     * Tries to get the absolute path for a command in the PATH.
     */
    @Nullable
    public static String locateExecutable(@NotNull final String command) {
        String whereCmd = (SystemInfo.isWindows ? "where" : "which") + ' ' + command;
        String res = exec(whereCmd);
        if (res != null && res.isEmpty()) {
            LOG.info("Could not find " + command);
        }
        return res;
    }
}
