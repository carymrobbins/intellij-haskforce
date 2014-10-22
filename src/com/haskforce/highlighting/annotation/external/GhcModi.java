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
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

public class GhcModi {
    public final Project project;
    public final String workingDirectory;
    public final String path;
    public final String flags;
    private Process process;
    private BufferedReader input;
    private BufferedWriter output;
    public static final Pattern TYPE_SPLIT_REGEX = Pattern.compile(" :: ");
    private static Map<Project, GhcModi> instanceMap = new HashMap<Project, GhcModi>(0);

    public static GhcModi getInstance(Project project, String workingDirectory) {
        final String path = getPath(project);
        final String flags = getFlags(project);
        final GhcModi instance = instanceMap.get(project);
        if (instance != null) {
            if (instance.workingDirectory.equals(workingDirectory)
                    && instance.path.equals(path)
                    && instance.flags.equals(flags)) {
                return instance;
            }
            instance.kill();
        }
        final GhcModi newInstance = new GhcModi(project, workingDirectory, path, flags);
        instanceMap.put(project, newInstance);
        return newInstance;
    }

    private GhcModi(Project project, String workingDirectory, String path, String flags) {
        this.project = project;
        this.workingDirectory = workingDirectory;
        this.path = path;
        this.flags = flags;
    }

    private synchronized void kill() {
        if (process != null) {
            process.destroy();
        }
        try {
            input.close();
        } catch (IOException e) {
            // Ignored.
        }
        try {
            output.close();
        } catch (IOException e) {
            // Ignored.
        }
    }

    @Nullable
    public static String getPath(@NotNull Project project) {
        return ExecUtil.GHC_MODI_KEY.getPath(project);
    }

    @NotNull
    public static String getFlags(@NotNull Project project) {
        return ExecUtil.GHC_MODI_KEY.getFlags(project);
    }

    @Nullable
    public Problems check(@NotNull String file) {
        final String stdout = simpleExec("check " + file);
        return stdout == null ? new Problems() : GhcMod.handleCheck(project, stdout, "ghc-modi");
    }

    /**
     * Returns an array of (name, type) pairs exposed for a given module.
     */
    @Nullable
    public List<Pair<String, String>> browse(@NotNull String module) {
        String[] lines = simpleExecToLines("browse -d " + module);
        if (lines == null) {
            return null;
        }
        List<Pair<String, String>> result = new ArrayList<Pair<String, String>>(lines.length);
        for (String line : lines) {
            final String[] parts = TYPE_SPLIT_REGEX.split(line, 2);
            //noinspection ObjectAllocationInLoop
            result.add(new Pair<String, String>(parts[0], parts.length == 2 ? parts[1] : ""));
        }
        return result;
    }

    @Nullable
    public String simpleExec(@NotNull String command) {
        final String path = getPath(project);
        final String stdout;
        if (path == null
                || (stdout = exec(command)) == null
                || stdout.length() == 0) {
            return null;
        }
        return stdout;
    }

    @Nullable
    public String[] simpleExecToLines(@NotNull String command) {
        final String result = simpleExec(command);
        return result == null ? null : StringUtil.splitByLines(result);
    }

    @Nullable
    public synchronized String exec(@NotNull String command) {
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
