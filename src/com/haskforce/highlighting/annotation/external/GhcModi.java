package com.haskforce.highlighting.annotation.external;

import com.haskforce.highlighting.annotation.Problems;
import com.haskforce.utils.ExecUtil;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.ParametersList;
import com.intellij.openapi.project.Project;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.util.Scanner;

public class GhcModi {
    private static Process process;
    private static BufferedReader input;
    private static BufferedWriter output;

    @Nullable
    public static String getPath(@NotNull Project project) {
        return ExecUtil.GHC_MODI_KEY.getPath(project);
    }

    @NotNull
    public static String getFlags(@NotNull Project project) {
        return ExecUtil.GHC_MODI_KEY.getFlags(project);
    }

    @Nullable
    public static Problems check(@NotNull Project project, @NotNull String workingDirectory, @NotNull String file) {
        final String stdout = simpleExec(project, workingDirectory, getFlags(project), "check " + file);
        return stdout == null ? new Problems() : GhcMod.handleCheck(project, stdout, "ghc-modi");
    }

    @Nullable
    public static String simpleExec(@NotNull Project project, @NotNull String workingDirectory,
                                    @NotNull String flags, @NotNull String command) {
        final String path = getPath(project);
        final String stdout;
        if (path == null
                || (stdout = exec(project, workingDirectory, path, flags, command)) == null
                || stdout.length() == 0) {
            return null;
        }
        return stdout;
    }

    @Nullable
    public static String exec(@NotNull Project project, @NotNull String workingDirectory, @NotNull String path,
                              @NotNull String flags, @NotNull String command) {
        if (process == null) {
            GeneralCommandLine commandLine = new GeneralCommandLine(path);
            ParametersList parametersList = commandLine.getParametersList();
            parametersList.addParametersString(flags);
            commandLine.setWorkDirectory(workingDirectory);
            // Make sure we can actually see the errors.
            commandLine.setRedirectErrorStream(true);
            try {
                process = commandLine.createProcess();
            } catch (com.intellij.execution.ExecutionException e) {
                // Notify the user that something terrible has happened.
                GhcMod.displayError(project, e.toString(), "ghc-modi");
                return null;
            }
            input = new BufferedReader(new InputStreamReader(process.getInputStream()));
            output = new BufferedWriter(new OutputStreamWriter(process.getOutputStream()));
        }
        try {
            output.write(command + System.getProperty("line.separator"));
            output.flush();
            StringBuilder builder = new StringBuilder(0);
            String line = input.readLine();
            while (line != null && !line.startsWith("OK") && !line.startsWith("NG")) {
                builder.append(line);
                builder.append(System.getProperty("line.separator"));
                line = input.readLine();
            }
            if (line != null && line.startsWith("NG")) {
                GhcMod.displayError(project, line, "ghc-modi");
                return null;
            }
            return builder.toString();
        } catch (IOException e) {
            GhcMod.displayError(project, e.toString(), "ghc-modi");
            return null;
        }
    }
}
