package com.haskforce.highlighting.annotation.external;

import com.haskforce.actions.RestartGhcModi;
import com.haskforce.highlighting.annotation.Problems;
import com.haskforce.utils.ExecUtil;
import com.haskforce.utils.NotificationUtil;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.ParametersList;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleComponent;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.xml.util.XmlUtil;
import org.apache.commons.lang.SystemUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.util.*;
import java.util.regex.Pattern;

/**
 * Process wrapper for GhcModi.  Implements ModuleComponent so destruction of processes coincides with closing projects.
 */
public class GhcModi implements ModuleComponent {
    @SuppressWarnings("UnusedDeclaration")
    private static final Logger LOG = Logger.getInstance(GhcModi.class);

    public @NotNull final Module module;
    public @NotNull String workingDirectory;
    public @Nullable String path;
    public @NotNull String flags;
    private @Nullable Process process;
    private @Nullable BufferedReader input;
    private @Nullable BufferedWriter output;
    private boolean enabled = true;
    // Keep track of error messages so we don't output the same ones multiple times.
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
     * Checks the module with ghc-modi and returns Problems to be annotated in the source.
     */
    @Nullable
    public Problems check(final @NotNull String file) {
        return handleErrors(new GhcModiCallable<Problems>() {
            @Override
            public Problems call() throws GhcModiError {
                final String stdout = simpleExec("check " + file);
                return stdout == null ? new Problems() : handleCheck(file, stdout);
            }
        });
    }

    @Nullable
    private static Problems handleCheck(@NotNull String file, @NotNull String stdout) throws GhcModiError {
        final Problems problems = GhcMod.parseProblems(new Scanner(stdout));
        if (problems == null) {
            // parseProblems should have returned something, so let's just dump the output to the user.
            throw new CheckParseError(stdout);
        } else if (problems.size() == 1) {
            final GhcMod.Problem problem = (GhcMod.Problem)problems.get(0);
            if (problem.startLine == 0 && problem.startColumn == 0) {
                throw new CheckError(file, problem.message);
            }
        }
        return problems;
    }

    /**
     * Returns an array of browse information for a given module.
     */
    @Nullable
    public BrowseItem[] browse(@NotNull final String module) {
        return handleErrors(new GhcModiCallable<BrowseItem[]>() {
            @Override
            public BrowseItem[] call() throws GhcModiError {
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
        });
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
    public String simpleExec(@NotNull String command) throws GhcModiError {
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
    public String[] simpleExecToLines(@NotNull String command) throws GhcModiError {
        final String result = simpleExec(command);
        return result == null ? null : StringUtil.splitByLines(result);
    }

    /**
     * Lazily spawns a process if needed and executes the given command.  If this fails, returns null and displays
     * the error to the user.  If the path to ghc-modi is not set, this simply returns null with no error message.
     */
    @Nullable
    public synchronized String exec(@NotNull String command) throws GhcModiError {
        if (!enabled) { return null; }
        ensureConsistent();
        if (path == null) { return null; }
        if (process == null) { spawnProcess(); }
        if (output == null) { throw new InitError("Output stream was unexpectedly null."); }
        if (input == null) { throw new InitError("Input stream was unexpectedly null."); }
        return interact(command, input, output);
    }

    private void spawnProcess() throws GhcModiError {
        GeneralCommandLine commandLine = new GeneralCommandLine(path);
        ParametersList parametersList = commandLine.getParametersList();
        parametersList.addParametersString(flags);
        // setWorkDirectory is deprecated but is needed to work with IntelliJ 13 which does not have withWorkDirectory.
        commandLine.setWorkDirectory(workingDirectory);
        // Make sure we can actually see the errors.
        commandLine.setRedirectErrorStream(true);
        try {
            process = commandLine.createProcess();
        } catch (ExecutionException e) {
            throw new InitError(e.toString());
        }
        input = new BufferedReader(new InputStreamReader(process.getInputStream()));
        output = new BufferedWriter(new OutputStreamWriter(process.getOutputStream()));
    }

    @Nullable
    private static String interact(@NotNull String command, @NotNull BufferedReader input, @NotNull BufferedWriter output) throws GhcModiError {
        try {
            // FIXME: A bit of a hack, but ensure that we don't accidentally read results from an unrelated command.
            final String unread = read(command, input, false);
            if (unread != null) { LOG.warn("Unread content found from ghc-modi, ignoring: " + unread); }
            output.write(command);
            output.newLine();
            output.flush();
            return read(command, input, true);
        } catch (IOException e) {
            try {
                // Attempt to grab an error message from ghc-modi.  It may throw its own NG bug error.
                final String error = read(command, input, false);
                if (error != null) { throw new ExecError(command, error); }
            } catch (IOException _) {
                // Ignored.
            }
            // If we couldn't get an error message, dump the java error to the user.
            throw new ExecError(command, e.toString());
        }
    }

    @Nullable
    private static String read(@NotNull String command, @NotNull BufferedReader input, boolean waitForInput) throws IOException, GhcModiError {
        StringBuilder builder = new StringBuilder(0);
        if (waitForInput) {
            wait(command, input);
        } else if (!input.ready()) {
            return null;
        }
        String line;
        for (;;) {
            if (!input.ready()) { break; }
            line = input.readLine();
            if (line == null || line.equals("OK")) { break; }
            if (line.startsWith("NG")) { throw new ExecError(command, line); }
            builder.append(line).append(SystemUtils.LINE_SEPARATOR);
        }
        return builder.toString();
    }

    private static final int READ_TIMEOUT = 3000;
    private static final int READ_INTERVAL = 50;
    private static final int READ_MAX_TRIES = READ_TIMEOUT / READ_INTERVAL;

    private static void wait(@NotNull String command, @NotNull BufferedReader input) throws IOException, GhcModiError {
        int tries = 0;
        if (!input.ready()) {
            ++tries;
            if (tries > READ_MAX_TRIES) { throw new ExecError(command, "ghc-modi took to long to respond."); }
            try {
                Thread.sleep(READ_INTERVAL);
            } catch (InterruptedException e) {
                throw new ExecError(command, "ghc-modi sleep thread interrupted: " + e.toString());
            }
        }
    }

    /**
     * Restarts the ghc-modi process and runs the check command on file to ensure the process starts successfully.
     */
    public synchronized void restartAndCheck(@NotNull String file) {
        setEnabled(true);
        check(file);
    }

    private void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    @Nullable
    private synchronized <T> T handleErrors(GhcModiCallable<T> callable) {
        final T result;
        try {
            result = callable.call();
        } catch (GhcModiError e) {
            kill();
            setEnabled(false);
            displayError("Killing ghc-modi due to process failure.<br/><br/>" +
                         "You can restart it using <b>" + XmlUtil.escape(RestartGhcModi.MENU_PATH) + "</b><br/><br/>" +
                         e.message);
            return null;
        }
        return result;
    }

    private void displayError(@NotNull String message) {
        NotificationUtil.displayToolsNotification(NotificationType.ERROR, module.getProject(), "ghc-modi error", message);
    }

    static interface GhcModiCallable<V> {
        V call() throws GhcModiError;
    }

    static abstract class GhcModiError extends Throwable {
        // Using error to index errors since message might have extra information.
        final @NotNull String error;
        final @NotNull String message;

        GhcModiError(@NotNull String error, @NotNull String message) {
            this.error = error;
            this.message = message;
        }
    }

    static class InitError extends GhcModiError {
        InitError(@NotNull String error) {
            super(error, "Initializing ghc-modi failed with error: " + error);
        }
    }

    static class ExecError extends GhcModiError {
        ExecError(@NotNull String command, @NotNull String error) {
            super(error, "Executing ghc-modi command '" + command + "' failed with error: " + error);
        }
    }

    static class CheckParseError extends GhcModiError {
        CheckParseError(@NotNull String stdout) {
            super(stdout, "Unable to parse problems from ghc-modi: " + stdout);
        }
    }

    static class CheckError extends GhcModiError {
        CheckError(@NotNull String file, @NotNull String error) {
            super(error, "Error checking file '" + file + "' with ghc-modi: " + error);
        }
    }

    /**
     * Private constructor used during module component initialization.
     */
    @SuppressWarnings("UnusedDeclaration")
    private GhcModi(@NotNull Module module) {
        final Project project = module.getProject();
        this.module = module;
        this.path = lookupPath();
        this.flags = lookupFlags();
        this.workingDirectory = lookupWorkingDirectory();
    }

    @Nullable
    private String lookupPath() {
        return ExecUtil.GHC_MODI_KEY.getPath(module.getProject());
    }

    @NotNull
    private String lookupFlags() {
        return ExecUtil.GHC_MODI_KEY.getFlags(module.getProject());
    }

    @NotNull
    private String lookupWorkingDirectory() {
        return ExecUtil.guessWorkDir(module);
    }

    /**
     * Checks that all ghc-modi metadata is consistent with the current instance (path, flags, etc.).  If something
     * has changed, updates the instance accordingly.
     */
    private void ensureConsistent() {
        // Just used for equality checks.
        final String newPath = lookupPath();
        final String newFlags = lookupFlags();
        final String newWorkingDirectory = lookupWorkingDirectory();
        // Ensure that nothing has changed; if so, kill the existing process and re-spawn.
        if (!StringUtil.equals(newPath, path)
                || !newFlags.equals(flags)
                || !newWorkingDirectory.equals(workingDirectory)) {
            kill();
            path = newPath;
            flags = newFlags;
            workingDirectory = newWorkingDirectory;
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
