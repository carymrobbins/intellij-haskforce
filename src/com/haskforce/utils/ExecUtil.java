package com.haskforce.utils;

import scala.util.Either;
import scala.util.Left;
import scala.util.Right;

import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
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
import java.util.List;

/**
 * Helper class to perform execution related tasks, including locating programs.
 */
public class ExecUtil {
    // Messages go to the log available in Help -> Show log in finder.
    private final static Logger LOG = Logger.getInstance(ExecUtil.class);

    /**
     * Tries to get the absolute path for a command in the PATH.
     */
    @Nullable
    public static String locateExecutable(@NotNull final String exePath) {
        GeneralCommandLine cmdLine = new GeneralCommandLine(
            SystemInfo.isWindows ? "where" : "which"
        );
        cmdLine.addParameter(exePath);
        final ProcessOutput processOutput;
        try {
            processOutput = new CapturingProcessHandler(cmdLine).runProcess();
        } catch (ExecutionException e) {
            throw new RuntimeException(
                "Failed to execute command: " + cmdLine.getCommandLineString(),
                e
            );
        }
        final String stdout = processOutput.getStdout();
        final String[] lines = stdout.trim().split("\n");
        if (lines.length == 0) return null;
        return lines[0].trim();
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
        String homeDir = System.getProperty("user.home");
        List<String> paths = ContainerUtil.newArrayList();
        // Executables installed by stack.
        paths.add(homeDir + sep +  ".local" + sep + "bin");
        //noinspection StatementWithEmptyBody
        if (SystemInfo.isWindows) {
            // TODO: Add windows paths.
        } else {
            // Unix bin dirs.
            paths.add(homeDir + sep + "Library" + sep + "Haskell" + sep + "bin");
            paths.add(homeDir + sep + ".cabal" + sep + "bin");
            paths.add(sep + "usr" + sep + "bin");
            paths.add(sep + "usr" + sep + "local" + sep + "bin");
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
        if (module != null) return guessWorkDir(module);
        return getProjectPath(project);
    }

    @NotNull
    public static String guessWorkDir(@NotNull PsiFile file) {
        return guessWorkDir(file.getProject(), file.getVirtualFile());
    }

    @NotNull
    public static String guessWorkDir(@NotNull Module module) {
        final VirtualFile moduleFile = module.getModuleFile();
        final VirtualFile moduleDir = moduleFile == null ? null : moduleFile.getParent();
        if (moduleDir != null) return moduleDir.getPath();
        return getProjectPath(module.getProject());
    }

    @NotNull
    private static String getProjectPath(@NotNull Project project) {
        final String projectPath = project.getBasePath();
        if (projectPath == null) {
            // This shouldn't actually happen since the projectPath will only be null for
            // the default project, not a project which has a corresponding module.
            throw new RuntimeException(
                "Unable to guess work directory - project '" + project + "' does not have a " +
                "base directory"
            );
        }
        return projectPath;
    }

    /**
     * Executes commandLine, optionally piping input to stdin, and return stdout.
     */
    @NotNull
    public static Either<ExecError, String> readCommandLine(@NotNull GeneralCommandLine commandLine,
                                                            @Nullable String input) {
        Process process;
        try {
            process = commandLine.createProcess();
        } catch (ExecutionException e) {
            return new ExecError(
                "Failed to create process for command: " + commandLine.getCommandLineString(),
                e
            ).toLeft();
        }
        if (input != null) {
            BufferedWriter writer = new BufferedWriter(new OutputStreamWriter(process.getOutputStream()));
            try {
                writer.write(input);
                writer.flush();
                writer.close();
            } catch (IOException e) {
                return new ExecError(
                    "IO error when writing to command process: " + commandLine.getCommandLineString(),
                    e
                ).toLeft();
            }
        }
        ProcessOutput processOutput = new CapturingProcessHandler(process).runProcess();
        if (processOutput.getExitCode() != 0) {
            return new ExecError(
                "Nonzero exit status (" + processOutput.getExitCode() + ") " +
                "from command: " + commandLine.getCommandLineString() + "\n" +
                "Process stderr: " + processOutput.getStderr(),
                null
            ).toLeft();
        }
        return new Right<ExecError, String>(processOutput.getStdout());
    }

    @NotNull
    public static Either<ExecError, String> readCommandLine(@NotNull GeneralCommandLine commandLine) {
        return readCommandLine(commandLine, null);
    }

    @NotNull
    public static Either<ExecError, String> readCommandLine(@Nullable String workingDirectory, @NotNull String command, @NotNull String[] params, @Nullable String input) {
        GeneralCommandLine commandLine = new GeneralCommandLine(command);
        if (workingDirectory != null) {
            commandLine.setWorkDirectory(workingDirectory);
        }
        commandLine.addParameters(params);
        return readCommandLine(commandLine, input);
    }

    @NotNull
    public static Either<ExecError, String> readCommandLine(@Nullable String workingDirectory, @NotNull String command, @NotNull String... params) {
        return readCommandLine(workingDirectory, command, params, null);
    }

    public static class ExecError {
        private final @NotNull String message;
        private final @Nullable Throwable cause;

        public ExecError(@NotNull String message, @Nullable Exception cause) {
            this.cause = cause;
            this.message = message;
            // Use .warn() instead of .error() since the latter causes unit test failures.
            LOG.warn(message, cause);
        }

        public String getMessage() {
            final StringBuilder result = new StringBuilder(message);
            if (cause != null) result.append("\nCaused by: ").append(cause);
            return result.toString();
        }

        public <A> Left<ExecError, A> toLeft() {
            return new Left<ExecError, A>(this);
        }
    }
}
