package com.haskforce.highlighting.annotation.external;

import com.haskforce.settings.HaskellBuildSettings;
import com.intellij.openapi.editor.VisualPosition;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.containers.*;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.*;
import java.util.HashSet;


/**
 * This class should contain the common code between GhcMod and GhcModi. Right now this class
 * only contains static methods, as there is no 'state' in common between GhcMod and GhcModi.
 */
public class GhcModUtil {
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

        /** Attempts to parse errors from ghc-mod from the stdout to report to the user. */
        @Nullable
        public String getUserError() {
            List<GhcMod.Problem> problems = GhcMod.parseProblems(new Scanner(stdout));
            if (problems == null || problems.isEmpty()) return null;
            HashSet<String> messages = new HashSet<String>(problems.size());
            for (GhcMod.Problem problem : problems) {
                messages.add(StringUtil.join(Arrays.asList(
                        problem.file,
                        problem.startLine,
                        problem.startColumn,
                        problem.message
                ), ":"));
            }
            return StringUtil.join(messages, "\n\n");
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
}
