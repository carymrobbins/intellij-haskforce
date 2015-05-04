package com.haskforce.highlighting.annotation.external;

import com.haskforce.settings.HaskellBuildSettings;
import com.intellij.openapi.editor.VisualPosition;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.SystemInfo;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.util.containers.OrderedSet;
import org.jetbrains.annotations.NotNull;

import java.io.File;
import java.util.Map;
import java.util.Scanner;
import java.util.Set;


/**
 * This class should contain the common code between GhcMod and GhcModi. Right now this class
 * only contains static methods, as there is no 'state' in common between GhcMod and GhcModi.
 */
public class GhcUtil {
    /**
     * The warnings concerning allocating objects in loop are correct but should not incur a
     * noticeable performance hit. The amount of looping done is dependent on the output of
     * GhcModi and that is very limited, mostly 3 - 5 lines.
     */
    @SuppressWarnings("ObjectAllocationInLoop")
    static String handleTypeInfo(VisualPosition selectionStartPosition,
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
