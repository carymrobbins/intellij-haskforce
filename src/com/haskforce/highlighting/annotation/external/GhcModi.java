package com.haskforce.highlighting.annotation.external;

import com.haskforce.actions.RestartGhcModi;
import com.haskforce.codeInsight.BrowseItem;
import com.haskforce.highlighting.annotation.Problems;
import com.haskforce.highlighting.annotation.external.GhcModUtil.GhcVersionValidation;
import com.haskforce.settings.SettingsChangeNotifier;
import com.haskforce.settings.ToolKey;
import com.haskforce.settings.ToolSettings;
import com.haskforce.ui.tools.HaskellToolsConsole;
import com.haskforce.utils.ExecUtil;
import com.haskforce.utils.HtmlUtils;
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
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.xml.util.XmlUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.Option;

import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.*;
import java.util.concurrent.*;
import java.util.regex.Pattern;

/**
 * Process wrapper for GhcModi.  Implements ModuleComponent so destruction of processes coincides with closing projects.
 */
public class GhcModi implements ModuleComponent, SettingsChangeNotifier {

    public static Option<GhcModi> get(PsiElement element) {
        final Module module = ModuleUtilCore.findModuleForPsiElement(element);
        if (module == null) return Option.apply(null);
        return get(module);
    }

    public static Option<GhcModi> get(@NotNull Module module) {
        final GhcModi ghcModi = module.getComponent(GhcModi.class);
        if (ghcModi.isConfigured()) return Option.apply(ghcModi);
        return Option.apply(null);
    }

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
    private GhcVersionValidation ghcVersionValidation = GhcVersionValidation.PENDING_VALIDATION;
    private final HaskellToolsConsole toolConsole;
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
    public static <T> T getFuture(@NotNull Project project, @NotNull Future<T> future) {
        long timeout = ToolKey.getGhcModiResponseTimeout(project);
        try {
            return future.get(timeout, TimeUnit.MILLISECONDS);
        } catch (InterruptedException e) {
            LOG.warn(e);
            displayError(project, "ghc-modi was interrupted: " + e);
        } catch (java.util.concurrent.ExecutionException e) {
            LOG.warn(e);
            displayError(project, "ghc-modi execution was aborted: " + e);
        } catch (TimeoutException e) {
            String msg = "ghc-modi took too long to respond (waited " + timeout + " milliseconds)";
            LOG.warn(msg, e);
            HaskellToolsConsole.get(project).writeError(ToolKey.GHC_MODI_KEY, msg);
        }
        return null;
    }

    public boolean isConfigured() {
        return path != null && !path.trim().isEmpty();
    }

    /**
     * Checks the module with ghc-modi and returns Problems to be annotated in the source.
     */
    @Nullable
    public Future<Problems> check(final @NotNull String file) {
        return handleGhcModiCall(new GhcModiCallable<Problems>() {
            @Override
            public Problems call() throws GhcModiError {
                return unsafeCheck(file);
            }
        });
    }

    @Nullable
    public Problems syncCheck(final @NotNull String file) {
        return runSync(new GhcModiCallable<Problems>() {
            @Override
            public Problems call() throws GhcModiError {
                return unsafeCheck(file);
            }
        });
    }

    @Nullable
    private Problems unsafeCheck(final @NotNull String file) throws GhcModiError {
        // Check the file content so only call out to ghc-mod when the file changes.
        final String fileContent;
        try {
            fileContent = new String(Files.readAllBytes(Paths.get(file)), StandardCharsets.UTF_8);
        } catch (IOException e) {
            e.printStackTrace();
            throw new CheckError(file, e.getMessage());
        }

        // Check the cache to determine if the file changed.
        Pair<String, Problems> item = checkCache.get(file);
        if (item != null && item.first.equals(fileContent)) return item.second;

        // Call out to ghc-mod.
        final String stdout = simpleExec("check " + file);
        Problems result = stdout == null ? new Problems() : handleCheck(module, file, stdout);

        // Update the cache.
        checkCache.put(file, new Pair<>(fileContent, result));
        return result;
    }

    private ConcurrentHashMap<String, Pair<String, Problems>> checkCache = new ConcurrentHashMap<>();

    @Nullable
    public String[] syncLang() {
        return runSync(new GhcModiCallable<String[]>() {
            @Override
            public String[] call() throws GhcModiError {
                return unsafeLang();
            }
        });
    }

    @NotNull
    private String[] unsafeLang() throws GhcModiError {
        return simpleExecToLinesOrEmpty("lang");
    }

    @Nullable
    public String[] syncFlag() {
        return runSync(new GhcModiCallable<String[]>() {
            @Override
            public String[] call() throws GhcModiError {
                return unsafeFlag();
            }
        });
    }

    @NotNull
    private String[] unsafeFlag() throws GhcModiError {
        return simpleExecToLinesOrEmpty("flag");
    }

    @Nullable
    public String[] syncList() {
        return runSync(new GhcModiCallable<String[]>() {
            @Override
            public String[] call() throws GhcModiError {
                return unsafeList();
            }
        });
    }

    @NotNull
    private String[] unsafeList() throws GhcModiError {
        if (listCache != null) return listCache;
        listCache = simpleExecToLinesOrEmpty("list");
        return listCache;
    }

    private String[] listCache = null;

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
                    NotificationUtil.displayToolsNotification(
                      NotificationType.ERROR, module.getProject(), "Type Info Error",
                      "There was an error when executing the ghc-modi `type` command:\n\n" + stdout);
                    return null;
                }
            }
        });
    }

    public String[] find(@NotNull final String symbol) {
        // Check to see if we've already found this symbol.
        String[] item = findCache.get(symbol);
        if (item != null && item.length > 0) return item;

        return runSync(new GhcModiCallable<String[]>() {
            @Override
            public String[] call() throws GhcModiError {
                final String command = "find " + symbol;
                String[] result = simpleExecToLinesOrEmpty(command);
                // Update the cache so we don't have to find this symbol again.
                findCache.put(symbol, result);
                return result;
            }
        });
    }

    private ConcurrentHashMap<String, String[]> findCache = new ConcurrentHashMap<>();

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
                return unsafeBrowse(module);
            }
        });
    }

    @Nullable
    public BrowseItem[] syncBrowse(@NotNull final String module) {
        return runSync(new GhcModiCallable<BrowseItem[]>() {
            @Override
            public BrowseItem[] call() throws GhcModiError {
                return unsafeBrowse(module);
            }
        });
    }

    @NotNull
    private BrowseItem[] unsafeBrowse(@NotNull final String module) throws GhcModiError {
        // Check the cache to avoid calling out to ghc-mod if we don't have to.
        BrowseItem[] item = browseCache.get(module);
        if (item != null && item.length > 0) return item;

        String[] lines = simpleExecToLines("browse -d " + module);
        if (lines == null) return new BrowseItem[0];
        BrowseItem[] result = new BrowseItem[lines.length];
        for (int i = 0; i < lines.length; ++i) {
            final String[] parts = TYPE_SPLIT_REGEX.split(lines[i], 2);
            //noinspection ObjectAllocationInLoop
            result[i] = new BrowseItem(parts[0], module, parts.length == 2 ? parts[1] : "");
        }

        // Update the cache in case we browse this module again.
        browseCache.put(module, result);
        return result;
    }

    private ConcurrentHashMap<String, BrowseItem[]> browseCache = new ConcurrentHashMap<>();

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

    @NotNull
    public String[] simpleExecToLinesOrEmpty(@NotNull String command) throws GhcModiError {
        final String[] result = simpleExecToLines(command);
        return result == null ? new String[] {} : result;
    }

    /**
     * Lazily spawns a process if needed and executes the given command.  If this fails, returns null and displays
     * the error to the user.  If the path to ghc-modi is not set, this simply returns null with no error message.
     */
    @Nullable
    public synchronized String exec(@NotNull String command) throws GhcModiError {
        lastCallTimeMillis = System.currentTimeMillis();
        if (!enabled) { return null; }
        if (path == null) { return null; }
        if (!validateGhcVersion()) { return null; }
        if (process == null) { spawnProcess(); }
        if (output == null) { throw new InitError("Output stream was unexpectedly null."); }
        if (input == null) { throw new InitError("Input stream was unexpectedly null."); }
        return interact(command, input, output);
    }

    private void spawnProcess() throws GhcModiError {
        GeneralCommandLine commandLine = new GeneralCommandLine(
            GhcModUtil.changedPathIfStack(module.getProject(), path)
        );
        GhcModUtil.updateEnvironment(module.getProject(), commandLine.getEnvironment());
        ParametersList parametersList = commandLine.getParametersList();
        parametersList.addParametersString(
            GhcModUtil.changedFlagsIfStack(module.getProject(), path, flags)
        );
        // setWorkDirectory is deprecated but is needed to work with IntelliJ 13 which does not have withWorkDirectory.
        commandLine.setWorkDirectory(workingDirectory);
        // Make sure we can actually see the errors.
        commandLine.setRedirectErrorStream(true);
        toolConsole.writeInput(ToolKey.GHC_MODI_KEY, "Using working directory: " + workingDirectory);
        toolConsole.writeInput(ToolKey.GHC_MODI_KEY, "Starting ghc-modi process: " + commandLine.getCommandLineString());
        try {
            process = commandLine.createProcess();
        } catch (ExecutionException e) {
            toolConsole.writeError(ToolKey.GHC_MODI_KEY, "Failed to initialize process");
            throw new InitError(e.toString());
        }
        input = new BufferedReader(new InputStreamReader(process.getInputStream()));
        output = new BufferedWriter(new OutputStreamWriter(process.getOutputStream()));
        startIdleKillerThread();
    }

    // Initialized by `startIdleKillerThread`
    private long lastCallTimeMillis;

    private void startIdleKillerThread() {
        lastCallTimeMillis = System.currentTimeMillis();
        long timeout = ToolKey.getGhcModiKillIdleTimeout(this.module.getProject());
        // Disable the idle killer if the configured timeout is zero or negative.
        if (timeout <= 0) return;
        ScheduledExecutorService scheduler = Executors.newScheduledThreadPool(1);
        // Task to kill process if it's been running idle for longer than the configured timeout.
        Runnable idleKiller = () -> {
            long diff = System.currentTimeMillis() - lastCallTimeMillis;
            if (diff > timeout) {
                if (lastCallTimeMillis == 0) {
                    LOG.info("ghc-modi in module '" + module.getName() + "'" +
                             " has not been called for at least " + timeout + " ms, killing");
                } else {
                    LOG.info("ghc-modi in module '" + module.getName() + "'" +
                             " has been idle for " + diff + " ms, killing");
                }
                // Shut down the scheduler, no longer needs to run.
                scheduler.shutdown();
                // Kill the ghc-modi process.
                kill();
            } else {
                LOG.info("ghc-modi in module '" + module.getName() + "'" +
                         " has only been idle for " + diff + " ms, allowed to stay alive");
            }
        };
        // Start the killer, delaying for the timeout to give ghc-modi time to be called, but then checking
        // every time out in case it needs to be killed in case it's been idle for too long.
        LOG.info("Starting scheduler to check ghc-modi idle time for module '" + module.getName() + "' every " + timeout + " ms");
        scheduler.scheduleAtFixedRate(idleKiller, timeout, timeout, TimeUnit.MILLISECONDS);
    }

    private boolean validateGhcVersion() {
        if (path == null) throw new RuntimeException("Unexpected GhcModi.path == null");
        ghcVersionValidation = GhcModUtil.validateGhcVersion(ghcVersionValidation, module.getProject(), path, flags);
        return ghcVersionValidation == GhcVersionValidation.VALID;
    }

    /**
     * Kills the existing process and closes input and output if they exist.
     */
    private synchronized void kill() {
        ghcVersionValidation = GhcVersionValidation.PENDING_VALIDATION;
        if (process != null) process.destroy();
        process = null;
        try { if (input != null) input.close(); } catch (IOException e) { /* Ignored */ }
        input = null;
        try { if (output != null) output.close(); } catch (IOException e) { /* Ignored */ }
        output = null;
    }

    @Nullable
    private String interact(@NotNull String command, @NotNull BufferedReader input, @NotNull BufferedWriter output) throws GhcModiError {
        toolConsole.writeInput(ToolKey.GHC_MODI_KEY, command);
        write(command, input, output);
        try {
            final String result = read(command, input);
            toolConsole.writeOutput(ToolKey.GHC_MODI_KEY, result);
            return result;
        } catch (InterruptedException e) {
            final ExecError err = new ExecError(command, e.toString());
            toolConsole.writeError(ToolKey.GHC_MODI_KEY, err.message);
            throw err;
        } catch (IOException e) {
            final ExecError err = new ExecError(command, e.toString());
            toolConsole.writeError(ToolKey.GHC_MODI_KEY, err.message);
            throw err;
        }
    }

    private void write(@NotNull String command, @NotNull BufferedReader input, @NotNull BufferedWriter output) throws GhcModiError {
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
            if (message == null || message.trim().isEmpty()) message = e.toString();
            throw new ExecError(command, messagePrefix + message);
        }
    }

    @Nullable
    private String read(@NotNull String command, @NotNull BufferedReader input) throws GhcModiError, IOException, InterruptedException {
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
        // Clear the caches.
        browseCache.clear();
        findCache.clear();
        checkCache.clear();
        listCache = null;

        // Kill the process, allowing it to be started up again.
        kill();
        setEnabled(true);
    }

    private void setEnabled(boolean enabled) {
        this.enabled = enabled;
    }

    @Nullable
    private <T> T runSync(final GhcModiCallable<T> callable) {
        try {
            return callable.call();
        } catch (final GhcModiError e) {
            final String messagePrefix;
            if (e.killProcess) {
                kill();
                setEnabled(false);
                messagePrefix = KILLING_MESSAGE_PREFIX;
            } else {
                messagePrefix = "";
            }
            ApplicationManager.getApplication().invokeLater(new Runnable() {
                @Override
                public void run() {
                    displayError(messagePrefix + e.message, true);
                }
            });
            return null;
        }
    }

    public static String KILLING_MESSAGE_PREFIX =
      "Killing ghc-modi due to process failure.<br/><br/>You can restart it using "
        + "<b>" + XmlUtil.escape(RestartGhcModi.MENU_PATH) + "</b><br/><br/>";

    @Nullable
    private synchronized <T> Future<T> handleGhcModiCall(final GhcModiCallable<T> callable) {
        return executorService.submit(new Callable<T>() {
            @Override
            public T call() {
                return runSync(callable);
            }
        });
    }

    private void displayError(@NotNull String message, boolean stripHtmlTags) {
        displayError(module.getProject(), message, stripHtmlTags);
    }

    private void displayError(@NotNull String message) {
        displayError(message, false);
    }

    private static void displayError(@NotNull Project project, @NotNull String message, boolean stripHtmlTags) {
        HaskellToolsConsole.get(project).writeError(ToolKey.GHC_MODI_KEY,
          stripHtmlTags ? HtmlUtils.stripTags(message) : message
        );
        NotificationUtil.displayToolsNotification(NotificationType.ERROR, project, "ghc-modi error", message);
    }

    private static void displayError(@NotNull Project project, @NotNull String message) {
        displayError(project, message, false);
    }

    interface GhcModiCallable<V> extends Callable<V> {
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

        @NotNull
        @Override
        public String getMessage() {
            return message;
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
        toolConsole = HaskellToolsConsole.get(module.getProject());
    }

    @Override
    public void onSettingsChanged(@NotNull ToolSettings settings) {
        this.path = settings.getPath();
        this.flags = settings.getFlags();
        toolConsole.writeError(ToolKey.GHC_MODI_KEY, "Settings changed, reloading ghc-modi, will spawn once invoked");
        kill();
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
