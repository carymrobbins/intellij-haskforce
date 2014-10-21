package com.haskforce.utils;

import com.haskforce.HaskellSdkType;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.execution.process.ProcessOutput;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.project.ProjectManager;
import com.intellij.openapi.util.Key;
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
import java.util.Map;

/**
 * Helper class to perform execution related tasks, including locating programs.
 */
public class ExecUtil {
    // Messages go to the log available in Help -> Show log in finder.
    private final static Logger LOG = Logger.getInstance(HaskellSdkType.class);

    /**
     * Execute a command using the default shell.
     */
    @Nullable
    public static String exec(@NotNull final String command) {
        // Find some valid working directory, doesn't matter which one.
        ProjectManager pm = ProjectManager.getInstance();
        Project[] projects = pm == null ? null : pm.getOpenProjects();
        String workDir = ".";
        // Set the working directory if there is an open project.
        if (pm == null || projects.length == 0) {
            LOG.info("No open projects so cannot find a valid path. Using '.'.");
        } else {
            workDir = projects[0].getBaseDir().getCanonicalPath();
        }
        return exec(workDir, command);
    }

    /**
     * Execute a command using the default shell in a given work directory.
     */
    @Nullable
    public static String exec(@NotNull final String workDir, @NotNull final String command) {
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

        ProcessOutput output = null;
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

        List<String> lines = output.getStdoutLines();
        StringBuilder sb = new StringBuilder(100*lines.size());
        for (String line : lines) {
            sb.append(line);
        }
        return sb.toString();
    }

    /**
     * Tries to get the absolute path for a command in the PATH.
     */
    @Nullable
    public static String locateExecutable(@NotNull final String command) {
        String whereCmd = (SystemInfo.isWindows ? "where" : "which") + ' ' + command;
        String res = exec(whereCmd);
        if (res != null && res.isEmpty()) {
            LOG.info("Could not find " + command);
        }
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
            if (new File(cmd).canExecute()) return cmd;
        }
        return null;
    }

    @NotNull
    public static String guessWorkDir(@NotNull Project project, @NotNull VirtualFile file) {
        final Module module = ModuleUtilCore.findModuleForFile(file, project);
        final VirtualFile moduleFile = module == null ? null : module.getModuleFile();
        final VirtualFile moduleDir = moduleFile == null ? null : moduleFile.getParent();
        return moduleDir == null ? project.getBasePath() : moduleDir.getPath();
    }

    @NotNull
    public static String guessWorkDir(@NotNull PsiFile file) {
        return guessWorkDir(file.getProject(), file.getVirtualFile());
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
            process.waitFor();
            output = new CapturingProcessHandler(process).runProcess().getStdout();
        } catch (ExecutionException e) {
            LOG.debug(e);
        } catch (InterruptedException e) {
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

    /**
     * String wrapper to ensure that we don't accidentally pass an invalid key to the PropertiesComponent.
     * These are set as property keys in HaskellToolsConfigurable.
     */
    public static class ToolKey {
        public final String pathKey;
        public final String flagsKey;

        ToolKey(String name) {
            this.pathKey = name + "Path";
            this.flagsKey = name + "Flags";
        }

        @Nullable
        public String getPath(@NotNull Project project) {
            final String path = PropertiesComponent.getInstance(project).getValue(pathKey);
            return path == null || path.isEmpty() ? null : path;
        }

        @NotNull
        public String getFlags(@NotNull Project project) {
            final String flags = PropertiesComponent.getInstance(project).getValue(flagsKey);
            return flags == null ? "" : flags;
        }
    }

    public static final ToolKey PARSER_HELPER_KEY = new ToolKey("parserHelper");
    public static final ToolKey STYLISH_HASKELL_KEY = new ToolKey("stylishHaskell");
    public static final ToolKey HLINT_KEY = new ToolKey("hlint");
    public static final ToolKey GHC_MOD_KEY = new ToolKey("ghcMod");

    public static class GhcModiToolKey extends ToolKey {
        public static final String useKey = "useGhcModi";
        public static final String trueValue = "true";

        GhcModiToolKey() {
            super("ghcModi");
        }

        public static boolean isEnabledFor(@NotNull Project project) {
            return trueValue.equals(PropertiesComponent.getInstance(project).getValue(useKey));
        }
    }

    public static final GhcModiToolKey GHC_MODI_KEY = new GhcModiToolKey();

    public static final Key<String> MODULE_CACHE_KEY = new Key("MODULE_CACHE");
    public static final Key<List<LookupElement>> LANGUAGE_CACHE_KEY = new Key("LANGUAGE_CACHE");
    public static final Key<String[]> FLAG_CACHE_KEY = new Key("FLAG_CACHE");
    public static final Key<Map<String, List<LookupElement>>> QUALIFIED_CACHE_KEY = new Key("QUALIFIED_CACHE");
}
