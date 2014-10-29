package com.haskforce.highlighting.annotation.external;

import com.haskforce.highlighting.annotation.Problems;
import com.haskforce.utils.ExecUtil;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.ParametersList;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleComponent;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.util.regex.Pattern;

public class GhcModi implements ModuleComponent {
    @SuppressWarnings("UnusedDeclaration")
    private static final Logger LOG = Logger.getInstance(GhcMod.class);

    public @NotNull final Module module;
    public @NotNull String workingDirectory;
    public @Nullable String path;
    public @NotNull String flags;
    private @Nullable Process process;
    private @Nullable BufferedReader input;
    private @Nullable BufferedWriter output;
    public static final Pattern TYPE_SPLIT_REGEX = Pattern.compile(" :: ");
//    private static @NotNull Map<Module, GhcModi> instanceMap = new HashMap<Module, GhcModi>(0);

    private synchronized void kill() {
//        instanceMap.remove(module);
        if (process != null) {
            process.destroy();
            process = null;
        }
        try {
            if (input != null) {
                input.close();
                input = null;
            }
        } catch (IOException e) {
            // Ignored.
        }
        try {
            if (output != null) {
                output.close();
                output = null;
            }
        } catch (IOException e) {
            // Ignored.
        }
    }

    private synchronized void killAndDisplayError(String command, String error) {
        kill();
        final String message = "Command: " + command + "<br/>Error: " + error;
        GhcMod.displayError(module.getProject(), message, "ghc-modi");
    }

    @Nullable
    public Problems check(@NotNull String file) {
        final String stdout = simpleExec("check " + file);
        return stdout == null ? new Problems() : GhcMod.handleCheck(module.getProject(), stdout, "ghc-modi");
    }

    /**
     * Returns an array of (name, type) pairs exposed for a given module.
     */
    @Nullable
    public BrowseItem[] browse(@NotNull final String module) {
        String[] lines = simpleExecToLines("browse -d " + module);
        if (lines == null) {
            return null;
        }
        BrowseItem[] result = new BrowseItem[lines.length];
        for (int i = 0; i < lines.length; ++i) {
            final String[] parts = TYPE_SPLIT_REGEX.split(lines[i], 2);
            //noinspection ObjectAllocationInLoop
            result[i] = new BrowseItem(parts[0], module, parts.length == 2 ? parts[1] : "");
        }
        return result;
    }

    public static class BrowseItem {
        public final @NotNull String name;
        public final @NotNull String module;
        public final @NotNull String type;

        public BrowseItem(@NotNull String name, @NotNull String module, @NotNull String type) {
            this.name = name;
            this.module = module;
            this.type = type;
        }

    }

    @Nullable
    public String simpleExec(@NotNull String command) {
        final String stdout = exec(command);
        if (stdout == null || stdout.length() == 0) {
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
        if (path == null) {
            return null;
        }
        ensureConsistent();
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
                killAndDisplayError(command, e.toString());
                return null;
            }
            input = new BufferedReader(new InputStreamReader(process.getInputStream()));
            output = new BufferedWriter(new OutputStreamWriter(process.getOutputStream()));
        }
        try {
            if (output == null) {
                killAndDisplayError(command, "Output stream was unexpectedly null.");
                return null;
            }
            if (input == null) {
                killAndDisplayError(command, "Input stream was unexpectedly null.");
                return null;
            }
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
                killAndDisplayError(command, line);
                return null;
            }
            return builder.toString();
        } catch (IOException e) {
            killAndDisplayError(command, e.toString());
            return null;
        }
    }

    @SuppressWarnings("UnusedDeclaration")
    private GhcModi(@NotNull Module module) {
        final Project project = module.getProject();
        this.module = module;
        this.path = ExecUtil.GHC_MODI_KEY.getPath(project);
        this.flags = ExecUtil.GHC_MODI_KEY.getFlags(project);
        this.workingDirectory = ExecUtil.guessWorkDir(module);
    }

    private void ensureConsistent() {
        // Just used for equality checks.
        GhcModi check = new GhcModi(module);
        // Ensure that nothing has changed; if so, kill the existing process and re-spawn.
        if (check.path == null
                || !check.path.equals(path)
                || !check.flags.equals(flags)
                || !check.workingDirectory.equals(workingDirectory)) {
            kill();
            path = check.path;
            flags = check.flags;
            workingDirectory = check.workingDirectory;
        }
    }

    // Implemented methods for ModuleComponent.

    @Override
    public void projectOpened() {
        // No need to do anything here.
    }

    @Override
    public void projectClosed() {
        kill();
    }

    @Override
    public void moduleAdded() {
        // No need to do anything here.
    }

    @Override
    public void initComponent() {
        // No need to do anything here.
    }

    @Override
    public void disposeComponent() {
        kill();
    }

    @NotNull
    @Override
    public String getComponentName() {
        return "GhcModi";
    }

}
