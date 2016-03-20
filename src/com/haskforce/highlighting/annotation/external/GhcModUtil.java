package com.haskforce.highlighting.annotation.external;

import com.haskforce.settings.HaskellBuildSettings;
import com.haskforce.utils.ExecUtil;
import com.haskforce.utils.NotificationUtil;
import com.haskforce.utils.EitherUtil;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.ParametersList;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.VisualPosition;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.containers.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.util.Either;

import java.io.File;
import java.util.*;
import java.util.HashSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


/**
 * This class should contain the common code between GhcMod and GhcModi. Right now this class
 * only contains static methods, as there is no 'state' in common between GhcMod and GhcModi.
 */
public class GhcModUtil {
    private static Logger LOG = Logger.getInstance(GhcModUtil.class);

    /**
     * Returns a String to display to the user as the type info.
     * If we are unable to parse the type info from ghc-mod, a
     * java.util.InputMismatchException might be thrown from the Scanner.
     */
    @SuppressWarnings("ObjectAllocationInLoop") // Should only be 3-5 loops.
    public static String unsafeHandleTypeInfo(VisualPosition selectionStartPosition,
                                              VisualPosition selectionStopPosition,
                                              @NotNull String stdout) {
        Scanner typeInfosScanner = new Scanner(stdout);
        String lineSeparator = System.getProperty("line.separator");
        typeInfosScanner.useDelimiter(lineSeparator);
        while (typeInfosScanner.hasNext()){
            Scanner typeInfoScanner = new Scanner(typeInfosScanner.next());
            typeInfoScanner.useDelimiter("\"");
            String rowAndColInfo = typeInfoScanner.next();
            Scanner rowAndColScanner = new Scanner(rowAndColInfo);
            int startRow = rowAndColScanner.nextInt();
            int startCol = rowAndColScanner.nextInt();
            int endRow   = rowAndColScanner.nextInt();
            int endCol   = rowAndColScanner.nextInt();
            String typeOnRowAndCol = typeInfoScanner.next();
            if (! (new VisualPosition(startRow, startCol).after(selectionStartPosition))
                    && ! selectionStopPosition.after(new VisualPosition(endRow, endCol))){
                typeInfosScanner.close();
                typeInfoScanner.close();
                rowAndColScanner.close();
                return typeOnRowAndCol;

            }
            typeInfoScanner.close();
            rowAndColScanner.close();
        }
        typeInfosScanner.close();
        return "No enclosing type found";
    }

    public static String handleTypeInfo(VisualPosition selectionStartPosition,
                                        VisualPosition selectionStopPosition,
                                        @NotNull String stdout) throws TypeInfoParseException {
        try {
            return unsafeHandleTypeInfo(selectionStartPosition, selectionStopPosition, stdout);
        } catch (InputMismatchException e) {
            throw new TypeInfoParseException(stdout, e);
        }
    }

    public static class TypeInfoParseException extends Exception {
        public final String stdout;

        public TypeInfoParseException(String stdout, Throwable cause) {
            super("Could not parse type info output", cause);
            this.stdout = stdout;
        }
    }

    /**
     * Updates the environment with path hacks so that ghc-mod(i) can find ghc, cabal, etc.
     */
    public static void updateEnvironment(@NotNull Project project, @NotNull Map<String, String> env) {
        HaskellBuildSettings settings = HaskellBuildSettings.getInstance(project);
        updateEnvironment(env, settings.getGhcPath(), settings.getCabalPath());
    }

    public static void updateEnvironment(@NotNull Map<String, String> env, String... paths) {
        Set<String> newPaths = new OrderedSet<String>();
        for (String path : paths) {
            //noinspection ObjectAllocationInLoop
            File exe = new File(path);
            if (exe.canExecute()) newPaths.add(exe.getParent());
        }
        String pathValue = env.get("PATH");
        if (pathValue != null && !pathValue.isEmpty()) newPaths.add(pathValue);
        newPaths.add(System.getenv("PATH"));
        env.put("PATH", StringUtil.join(newPaths, SystemInfo.isWindows ? ";" : ":"));
    }

    /**
     * If stack is enabled for the project, returns the path to stack.  The .getFlags
     * method will then take care of pointing to the correct ghc-mod executable.
     * Otherwise, the ghc-mod path is returned directly.  If ghc-mod is not configured,
     * the method returns null.
     */
    public static String changedPathIfStack(@NotNull Project project, @Nullable String ghcModPath) {
        if (ghcModPath == null) return null;
        String stackPath = GhcModUtil.maybeStackPath(project);
        if (stackPath == null) return ghcModPath;
        return stackPath;
    }

    /**
     * If stack is enabled for the project, returns the arguments needed to point stack
     * to ghc-mod, including any flags provided. Otherwise, returns the flags configured
     * for ghc-mod.
     */
    @NotNull
    public static String changedFlagsIfStack(@NotNull Project project,
                                             @Nullable String ghcModPath,
                                             @NotNull String ghcModFlags) {
        if (GhcModUtil.maybeStackPath(project) == null) {
            return ghcModFlags;
        }
        return "exec -- " + ghcModPath +  " " + ghcModFlags;
    }

    @NotNull
    public static GhcVersionValidation validateGhcVersion(@Nullable GhcVersionValidation v,
                                                          @NotNull Project project,
                                                          @NotNull String ghcModPath,
                                                          @NotNull String ghcModFlags) {
        if (v == null || v == GhcVersionValidation.PENDING_VALIDATION) {
            return validateGhcVersion(project, ghcModPath, ghcModFlags)
                ? GhcVersionValidation.VALID
                : GhcVersionValidation.INVALID;
        }
        return v;
    }

    private static boolean validateGhcVersion(@NotNull Project project,
                                              @NotNull String ghcModPath,
                                              @NotNull String ghcModFlags) {
        String workDir = project.getBasePath();
        if (workDir == null) {
            LOG.warn(
                "Project base path is null, unable to use it as the work directory for" +
                "ghc-mod version validation"
            );
        }
        GeneralCommandLine ghcModVersionCmdLine = new GeneralCommandLine(ghcModPath);
        ghcModVersionCmdLine.withWorkDirectory(workDir);
        ParametersList ghcModVersionParamsList = ghcModVersionCmdLine.getParametersList();
        ghcModVersionParamsList.addParametersString(ghcModFlags);
        ghcModVersionParamsList.add("--version");
        Either<ExecUtil.ExecError, String> ghcModVersionResult = ExecUtil.readCommandLine(ghcModVersionCmdLine);
        if (ghcModVersionResult.isLeft()) {
            //noinspection ThrowableResultOfMethodCallIgnored
            ExecUtil.ExecError e = EitherUtil.unsafeGetLeft(ghcModVersionResult);
            NotificationUtil.displayToolsNotification(NotificationType.ERROR, project, "ghc-mod",
                e.getMessage()
            );
            return false;
        }
        String ghcModVersionInfo = EitherUtil.unsafeGetRight(ghcModVersionResult).trim();
        Matcher m = GHC_VERSION_REGEX.matcher(ghcModVersionInfo);
        if (!m.find()) {
            NotificationUtil.displayToolsNotification(NotificationType.ERROR, project, "ghc-mod",
              "Could not find GHC version in ghc-mod version info: '" + ghcModVersionInfo + "'"
            );
            return false;
        }
        String ghcModGhcVersion = m.group(1);
        String stackPath = maybeStackPath(project);
        GeneralCommandLine ghcVersionCmdLine = new GeneralCommandLine();
        ghcVersionCmdLine.withWorkDirectory(workDir);
        HaskellBuildSettings settings = HaskellBuildSettings.getInstance(project);
        if (stackPath != null) {
            ghcVersionCmdLine.setExePath(stackPath);
            ghcVersionCmdLine.addParameters(
                "--stack-yaml", settings.getStackFile(),
                "ghc", "--", "--numeric-version"
            );
        } else {
            String ghcPath = settings.getGhcPath();
            ghcVersionCmdLine.setExePath(ghcPath);
            ghcVersionCmdLine.addParameter("--numeric-version");
        }
        Either<ExecUtil.ExecError, String> ghcVersionResult = ExecUtil.readCommandLine(ghcVersionCmdLine);
        if (ghcVersionResult.isLeft()) {
            //noinspection ThrowableResultOfMethodCallIgnored
            ExecUtil.ExecError e = EitherUtil.unsafeGetLeft(ghcVersionResult);
            NotificationUtil.displayToolsNotification(NotificationType.ERROR, project, "ghc",
                e.getMessage()
            );
            return false;
        }
        String ghcVersion = EitherUtil.unsafeGetRight(ghcVersionResult).trim();
        if (!ghcVersion.trim().equals(ghcModGhcVersion)) {
            NotificationUtil.displayToolsNotification(NotificationType.ERROR, project, "ghc-mod",
              "Attempting to use a ghc-mod compiled with a different version of ghc:\n" +
                "GHC version: '" + ghcVersion + "'\n" +
                "ghc-mod compiled with ghc version: '" + ghcModGhcVersion + "'\n" +
                "Please reconfigure ghc-mod to use a version compiled with GHC " + ghcVersion
            );
            return false;
        }
        return true;
    }

    /**
     * If stack is enabled for the project, return the stack path.  If this method
     * returns null, stack is not enabled for the project.
     */
    @Nullable
    private static String maybeStackPath(@NotNull Project project) {
        HaskellBuildSettings s = HaskellBuildSettings.getInstance(project);
        if (!s.isStackEnabled()) return null;
        return s.getStackPath();
    }

    /**
     * Used for parsing the GHC version from `ghc-mod --version`, for example -
     * $ ghc-mod --version
     * ghc-mod version 5.4.0.0 compiled by GHC 7.10.2
     */
    private static Pattern GHC_VERSION_REGEX = Pattern.compile("GHC (\\d+(\\.\\d+)+)");

    /**
     * Values for keeping track of ghc-mod processes' validation of their ghc version.
     */
    public enum GhcVersionValidation {
        PENDING_VALIDATION("PENDING_VALIDATION"),
        VALID("VALID"),
        INVALID("INVALID");

        private final String name;

        GhcVersionValidation(String name) {
            this.name = name;
        }

        @Override
        public String toString() {
            return getClass().getSimpleName() + '.' + name;
        }
    }
}
