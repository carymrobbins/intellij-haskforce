package com.haskforce.highlighting.annotation.external;

import com.haskforce.actions.RestartGhcModi;
import com.haskforce.highlighting.annotation.Problems;
import com.haskforce.settings.SettingsChangeNotifier;
import com.haskforce.settings.ToolKey;
import com.haskforce.settings.ToolSettings;
import com.haskforce.utils.ExecUtil;
import com.haskforce.utils.NotificationUtil;
import com.haskforce.utils.SystemUtil;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.ParametersList;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.VisualPosition;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleComponent;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.xml.util.XmlUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.regex.Pattern;

/**
 * Process wrapper for GhcModi.  Implements ModuleComponent so destruction of processes coincides with closing projects.
 */
public class GhcModi implements ModuleComponent, SettingsChangeNotifier {
    @SuppressWarnings("UnusedDeclaration")
    private static final Logger LOG = Logger.getInstance(GhcModi.class);

    public @NotNull final Module module;
    public @NotNull String workingDirectory;
    public @Nullable String path;
    public @NotNull String flags;
    private @Nullable Process process;
    private @Nullable BufferedReader input;
    private @Nullable BufferedWriter output;
    private ExecutorService executorService = Executors.newSingleThreadExecutor();
    private boolean enabled = true;
    // Keep track of error messages so we don't output the same ones multiple times.
    public static final Pattern TYPE_SPLIT_REGEX = Pattern.compile(" :: ");

    public static Problems getFutureProblems(@NotNull Project project, @NotNull Future<Problems> problemsFuture) {
        return getFuture(project, problemsFuture);
    }

    public static BrowseItem[] getFutureBrowseItems(@NotNull Project project, @NotNull Future<BrowseItem[]> browseItemsFuture) {
        return getFuture(project, browseItemsFuture);
    }

    public static String getFutureType(@NotNull Project project, @NotNull Future<String> typeFuture) {
        return getFuture(project, typeFuture);
    }


    @Nullable
    private static <T> T getFuture(@NotNull Project project, @NotNull Future<T> future) {
        long timeout = ToolKey.getGhcModiTimeout(project);
        try {
            return future.get(timeout, TimeUnit.MILLISECONDS);
        } catch (InterruptedException e) {
            LOG.warn(e);
            displayError(project, "ghc-modi was interrupted: " + e);
        } catch (java.util.concurrent.ExecutionException e) {
            LOG.warn(e);
            displayError(project, "ghc-modi execution was aborted: " + e);
        } catch (TimeoutException e) {
            LOG.warn("ghc-modi took too long to respond (waited " + timeout + " milliseconds): " + e);
            e.printStackTrace();
        }
        return null;
    }

    /**
     * Checks the module with ghc-modi and returns Problems to be annotated in the source.
     */
    @Nullable
    public Future<Problems> check(final @NotNull String file) {
        return handleGhcModiCall(new GhcModiCallable<Problems>() {
            @Override
            public Problems call() throws GhcModiError {
                final String stdout = simpleExec("check " + file);
                return stdout == null ? new Problems() : handleCheck(module, file, stdout);
            }
        });
    }

    public Future<String> type(final @NotNull String canonicalPath,
                               @NotNull final VisualPosition startPosition,
                               @NotNull final VisualPosition  stopPosition) {
        return handleGhcModiCall(new GhcModiCallable<String>(){
            @Override
            public String call() throws GhcModiError {
                final String command = "type " + canonicalPath + ' ' + startPosition.line + ' ' + startPosition.column;
                final String stdout = simpleExec(command);
                try {
                    return stdout == null ? "Type info not found" : GhcModUtil.handleTypeInfo(startPosition, stopPosition, stdout);
                } catch (GhcModUtil.TypeInfoParseException e) {
                    // If there's a user error, provide that via the tooltip.
                    String userError = e.getUserError();
                    // Otherwise, provide a notification for the error.
                    if (userError == null) throw new ExecError(command, stdout);
                    return userError;
                }
            }
        });
    }

    @Nullable
    private static Problems handleCheck(@NotNull Module module, @NotNull String file, @NotNull String stdout) throws GhcModiError {
        final Problems problems = GhcMod.parseProblems(module, new Scanner(stdout));
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
    public Future<BrowseItem[]> browse(@NotNull final String module) {
        return handleGhcModiCall(new GhcModiCallable<BrowseItem[]>() {
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
        if (path == null) { return null; }
        if (process == null) { spawnProcess(); }
        if (output == null) { throw new InitError("Output stream was unexpectedly null."); }
        if (input == null) { throw new InitError("Input stream was unexpectedly null."); }
        return interact(command, input, output);
    }

    private void spawnProcess() throws GhcModiError {
        GeneralCommandLine commandLine = new GeneralCommandLine(path);
        GhcModUtil.updateEnvironment(module.getProject(), commandLine.getEnvironment());
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

    /**
     * Kills the existing process and closes input and output if they exist.
     */
    private synchronized void kill() {
        if (process != null) process.destroy();
        process = null;
        try { if (input != null) input.close(); } catch (IOException e) { /* Ignored */ }
        input = null;
        try { if (output != null) output.close(); } catch (IOException e) { /* Ignored */ }
        output = null;
    }

    @Nullable
    private static String interact(@NotNull String command, @NotNull BufferedReader input, @NotNull BufferedWriter output) throws GhcModiError {
        write(command, input, output);
        try {
            return read(command, input);
        } catch (InterruptedException e) {
            throw new ExecError(command, e.toString());
        } catch (IOException e) {
            throw new ExecError(command, e.toString());
        }
    }

    private static void write(@NotNull String command, @NotNull BufferedReader input, @NotNull BufferedWriter output) throws GhcModiError {
        try {
            output.write(command);
            output.newLine();
            output.flush();
        } catch (IOException e) {
            final String messagePrefix = "Failed to write command to ghc-modi: ";
            String message = null;
            try {
                // Attempt to read error from ghc-modi.
                message = read(command, input);
            } catch (IOException ignored) {
                // Ignored
            } catch (InterruptedException ignored) {
                // Ignored
            }
            if (message == null) message = e.toString();
            throw new ExecError(command, messagePrefix + message);
        }
    }

    @Nullable
    private static String read(@NotNull String command, @NotNull BufferedReader input) throws GhcModiError, IOException, InterruptedException {
        StringBuilder builder = new StringBuilder(0);
        String line;
        for (;;) {
            line = input.readLine();
            if (line == null || line.equals("OK")) { break; }
            if (line.startsWith("NG")) { throw new ExecError(command, line); }
            builder.append(line).append(SystemUtil.LINE_SEPARATOR);
        }
        return builder.toString();
    }

    /**
     * Restarts the ghc-modi process and runs the check command on file to ensure the process starts successfully.
     */
    public synchronized void restart() {
        kill();
        setEnabled(true);
    }

    private void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    @Nullable
    private synchronized <T> Future<T> handleGhcModiCall(final GhcModiCallable<T> callable) {
        return executorService.submit(new Callable<T>() {
            @Override
            public T call() {
                try {
                    return callable.call();
                } catch (final GhcModiError e) {
                    final String messagePrefix;
                    if (e.killProcess) {
                        kill();
                        setEnabled(false);
                        messagePrefix = "Killing ghc-modi due to process failure.<br/><br/>You can restart it using " +
                                "<b>" + XmlUtil.escape(RestartGhcModi.MENU_PATH) + "</b><br/><br/>";
                    } else {
                        messagePrefix = "";
                    }
                    ApplicationManager.getApplication().invokeLater(new Runnable() {
                        @Override
                        public void run() {
                            displayError(messagePrefix + e.message);
                        }
                    });
                    return null;
                }
            }
        });
    }

    private void displayError(@NotNull String message) {
        displayError(module.getProject(), message);
    }

    private static void displayError(@NotNull Project project, @NotNull String message) {
        NotificationUtil.displayToolsNotification(NotificationType.ERROR, project, "ghc-modi error", message);
    }

    static interface GhcModiCallable<V> extends Callable<V> {
        V call() throws GhcModiError;
    }

    static abstract class GhcModiError extends Exception {
        // Using error to index errors since message might have extra information.
        final @NotNull String error;
        final @NotNull String message;
        final boolean killProcess;

        GhcModiError(@NotNull String error, @NotNull String message, boolean killProcess) {
            this.error = error;
            this.message = message;
            this.killProcess = killProcess;
        }
    }

    static class InitError extends GhcModiError {
        InitError(@NotNull String error) {
            super(error, "Initializing ghc-modi failed with error: " + error, true);
        }
    }

    static class ExecError extends GhcModiError {
        ExecError(@NotNull String command, @NotNull String error) {
            super(error, "Executing ghc-modi command '" + command + "' failed with error: " + error, true);
        }
    }

    static class CheckParseError extends GhcModiError {
        CheckParseError(@NotNull String stdout) {
            super(stdout, "Unable to parse problems from ghc-modi: " + stdout, false);
        }
    }

    static class CheckError extends GhcModiError {
        CheckError(@NotNull String file, @NotNull String error) {
            super(error, "Error checking file '" + file + "' with ghc-modi: " + error, false);
        }
    }

    /**
     * Private constructor used during module component initialization.
     */
    public GhcModi(@NotNull Module module) {
        this.module = module;
        this.path = lookupPath();
        this.flags = lookupFlags();
        this.workingDirectory = lookupWorkingDirectory();
        // Ensure that we are notified of changes to the settings.
        module.getProject().getMessageBus().connect().subscribe(SettingsChangeNotifier.GHC_MODI_TOPIC, this);
    }

    @Override
    public void onSettingsChanged(@NotNull ToolSettings settings) {
        this.path = settings.getPath();
        this.flags = settings.getFlags();
        kill();
        try {
            spawnProcess();
        } catch (GhcModiError e) {
            displayError(e.message);
        }
    }

    @Nullable
    private String lookupPath() {
        return ToolKey.GHC_MODI_KEY.getPath(module.getProject());
    }

    @NotNull
    private String lookupFlags() {
        return ToolKey.GHC_MODI_KEY.getFlags(module.getProject());
    }

    @NotNull
    private String lookupWorkingDirectory() {
        return ExecUtil.guessWorkDir(module);
    }

    // Implemented methods for ModuleComponent.

    @Override
    public void projectOpened() {
        // No need to do anything here.
    }

    @Override
    public void projectClosed() {
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
        executorService.shutdownNow();
        kill();
    }

    @NotNull
    @Override
    public String getComponentName() {
        return "GhcModi";
    }

}
