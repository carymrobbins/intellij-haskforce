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

/**
 * Process wrapper for GhcModi.  Implements ModuleComponent so destruction of processes coincides with closing projects.
 */
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

    /**
     * Kills the existing process and closes input and output if they exist.
     */
    private synchronized void kill() {
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

    /**
     * Helper when ghc-modi encounters an error; kill it and display the error to the user.
     */
    private synchronized void killAndDisplayError(String command, String error) {
        kill();
        final String message = "Command: " + command + "<br/>Error: " + error;
        GhcMod.displayError(module.getProject(), message, "ghc-modi");
    }

    /**
     * Checks the module with ghc-modi and returns Problems to be annotated in the source.
     */
    @Nullable
    public Problems check(@NotNull String file) {
        final String stdout = simpleExec("check " + file);
        return stdout == null ? new Problems() : GhcMod.handleCheck(module.getProject(), stdout, "ghc-modi");
    }

    /**
     * Returns an array of browse information for a given module.
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

    /**
     * Wrapper class for the output of `ghc-modi browse` command.
     */
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

    /**
     * A wrapper to exec that checks stdout and returns null if there was no output.
     */
    @Nullable
    public String simpleExec(@NotNull String command) {
        final String stdout = exec(command);
        if (stdout == null || stdout.length() == 0) {
            return null;
        }
        return stdout;
    }

    /**
     * Same as simpleExec, except returns an array of Strings for each line in the output.
     */
    @Nullable
    public String[] simpleExecToLines(@NotNull String command) {
        final String result = simpleExec(command);
        return result == null ? null : StringUtil.splitByLines(result);
    }

    /**
     * Lazily spawns a process if needed and executes the given command.  If this fails, returns null and displays
     * the error to the user.  If the path to ghc-modi is not set, this simply returns null with no error message.
     */
    @Nullable
    public synchronized String exec(@NotNull String command) {
        ensureConsistent();
        if (path == null) {
            return null;
        }
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

    /**
     * Private constructor used during module component initialization.
     */
    @SuppressWarnings("UnusedDeclaration")
    private GhcModi(@NotNull Module module) {
        final Project project = module.getProject();
        this.module = module;
        this.path = ExecUtil.GHC_MODI_KEY.getPath(project);
        this.flags = ExecUtil.GHC_MODI_KEY.getFlags(project);
        this.workingDirectory = ExecUtil.guessWorkDir(module);
    }

    /**
     * Checks that all ghc-modi metadata is consistent with the current instance (path, flags, etc.).  If something
     * has changed, updates the instance accordingly.
     */
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
