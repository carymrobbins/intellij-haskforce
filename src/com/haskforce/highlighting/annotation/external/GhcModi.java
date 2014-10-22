package com.haskforce.highlighting.annotation.external;

import com.haskforce.highlighting.annotation.Problems;
import com.haskforce.utils.ExecUtil;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.ParametersList;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

public class GhcModi {
    private static Process process;
    private static BufferedReader input;
    private static BufferedWriter output;
    public static final Pattern TYPE_SPLIT_REGEX = Pattern.compile(" :: ");

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
        final String stdout = simpleExec(project, workingDirectory, "check " + file);
        return stdout == null ? new Problems() : GhcMod.handleCheck(project, stdout, "ghc-modi");
    }

    /**
     * Returns an array of (name, type) pairs exposed for a given module.
     */
    @Nullable
    public static List<Pair<String, String>> browse(@NotNull Project project, @NotNull String workingDirectory, @NotNull String module) {
        String[] lines = simpleExecToLines(project, workingDirectory, "browse -d " + module);
        if (lines == null) {
            return null;
        }
        List<Pair<String, String>> result = new ArrayList<Pair<String, String>>(lines.length);
        for (String line : lines) {
            final String[] parts = TYPE_SPLIT_REGEX.split(line);
            //noinspection ObjectAllocationInLoop
            result.add(new Pair<String, String>(parts[0], parts[1]));
        }
        return result;
    }

    @Nullable
    public static String simpleExec(@NotNull Project project, @NotNull String workingDirectory, @NotNull String command) {
        final String path = getPath(project);
        final String stdout;
        if (path == null
                || (stdout = exec(project, workingDirectory, path, getFlags(project), command)) == null
                || stdout.length() == 0) {
            return null;
        }
        return stdout;
    }

    @Nullable
    public static String[] simpleExecToLines(@NotNull Project project, @NotNull String workingDirectory, @NotNull String command) {
        final String result = simpleExec(project, workingDirectory, command);
        return result == null ? null : StringUtil.splitByLines(result);
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
            } catch (ExecutionException e) {
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
