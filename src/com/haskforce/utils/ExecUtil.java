package com.haskforce.utils;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.BufferedWriter;
import java.io.File;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.nio.charset.Charset;
import java.util.List;

/**
 * Helper class to perform execution related tasks, including locating programs.
 */
public class ExecUtil {
    // Messages go to the log available in Help -> Show log in finder.
    private final static Logger LOG = Logger.getInstance(ExecUtil.class);

    /**
     * Execute a command using the default shell.
     */
    @Nullable
    public static String exec(@NotNull final String command) {
        List<String> lines = execMultiLine(command);
        if (lines == null) return null;
        StringBuilder sb = new StringBuilder(100 * lines.size());
        for (String line : lines) {
            sb.append(line);
        }
        return sb.toString();
    }

    /**
     * Execute a command using the default shell.
     */
    @Nullable
    public static List<String> execMultiLine(@NotNull final String command) {
        // Find some valid working directory, doesn't matter which one.
        ProjectManager pm = ProjectManager.getInstance();
        Project[] projects = pm == null ? null : pm.getOpenProjects();
        final String defaultWorkDir = ".";
        final String workDir;
        // Set the working directory if there is an open project.
        if (pm == null || projects.length == 0) {
            LOG.info("No open projects so cannot find a valid path. Using '.'.");
            workDir = defaultWorkDir;
        } else {
            workDir = projects[0].getBaseDir().getCanonicalPath();
        }
        return execMultiLine(workDir == null ? defaultWorkDir : workDir, command);
    }

    /**
     * Execute a command using the default shell in a given work directory.
     */
    @Nullable
    public static List<String> execMultiLine(@NotNull final String workDir, @NotNull final String command) {
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

        ProcessOutput output;
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

        return output.getStdoutLines();
    }

    /**
     * Tries to get the absolute path for a command in the PATH.
     */
    @Nullable
    public static String locateExecutable(@NotNull final String command) {
        String whereCmd = (SystemInfo.isWindows ? "where" : "which") + ' ' + command;
        List<String> lines = execMultiLine(whereCmd);
        if (lines == null || lines.isEmpty()) return null;
        String res = lines.get(0);
        if (res == null) return null;
        if (res.isEmpty()) LOG.info("Could not find '" + command + "' with ExecUtil.locateExecutable()");
        return res;
    }

    /**
     * Tries to get the absolute path for a command by defaulting to first
     * trying to locate the command in path, and falling back to trying likely
     * directories.
     */
    @Nullable
    public static String locateExecutableByGuessing(@NotNull final String command) {
        String located = locateExecutable(command);
        if (located != null && !located.isEmpty()) {
            // Found it!
            return located;
        }

        char sep = File.separatorChar;
        List<String> paths = ContainerUtil.newArrayList();
        if (SystemInfo.isWindows) {
            // TODO: Add windows paths.
        } else {
            String homeDir = System.getProperty("user.home");

            paths.add(homeDir + sep + "Library" + sep + "Haskell" + sep + "bin");
            paths.add(homeDir + sep + ".cabal" + sep + "bin");
            paths.add(sep + "usr" + "local" + sep + "bin");
            paths.add(homeDir + sep + "bin");
        }
        for (String path : paths) {
            String cmd = path + sep + command;
            //noinspection ObjectAllocationInLoop
            if (new File(cmd).canExecute()) return cmd;
        }
        return null;
    }

    @NotNull
    public static String guessWorkDir(@NotNull Project project, @NotNull VirtualFile file) {
        final Module module = ModuleUtilCore.findModuleForFile(file, project);
        return module == null ? project.getBasePath() : guessWorkDir(module);
    }

    @NotNull
    public static String guessWorkDir(@NotNull PsiFile file) {
        return guessWorkDir(file.getProject(), file.getVirtualFile());
    }

    @NotNull
    public static String guessWorkDir(@NotNull Module module) {
        final VirtualFile moduleFile = module.getModuleFile();
        final VirtualFile moduleDir = moduleFile == null ? null : moduleFile.getParent();
        return moduleDir == null ? module.getProject().getBasePath() : moduleDir.getPath();
    }

    /**
     * Executes commandLine, optionally piping input to stdin, and return stdout.
     */
    @Nullable
    public static String readCommandLine(@NotNull GeneralCommandLine commandLine, @Nullable String input) {
        String output = null;
        try {
            Process process = commandLine.createProcess();
            if (input != null) {
                BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(process.getOutputStream()));
                writer.write(input);
                writer.flush();
                writer.close();
            }
            output = new CapturingProcessHandler(process).runProcess().getStdout();
        } catch (ExecutionException e) {
            LOG.debug(e);
        } catch (IOException e) {
            LOG.debug(e);
        }
        return output;
    }

    @Nullable
    public static String readCommandLine(@NotNull GeneralCommandLine commandLine) {
        return readCommandLine(commandLine, null);
    }

    @Nullable
    public static String readCommandLine(@Nullable String workingDirectory, @NotNull String command, @NotNull String[] params, @Nullable String input) {
        GeneralCommandLine commandLine = new GeneralCommandLine(command);
        if (workingDirectory != null) {
            commandLine.setWorkDirectory(workingDirectory);
        }
        commandLine.addParameters(params);
        return readCommandLine(commandLine, input);
    }

    @Nullable
    public static String readCommandLine(@Nullable String workingDirectory, @NotNull String command, @NotNull String... params) {
        return readCommandLine(workingDirectory, command, params, null);
    }
}
