package com.haskforce.utils;

import com.intellij.openapi.util.SystemInfo;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class ExecUtil {

    /**
     * Execute a command using the default shell.
     */
    @Nullable
    public static String exec(@NotNull final String command) {
        String shell;
        if (SystemInfo.isWindows) {
            shell = "cmd /c";
        } else {
            // Default to UNIX if not Windows.
            shell = "sh -c";
        }
        return run(shell + ' ' + command);
    }

    /**
     * Execute a command without any surrounding shell.
     */
    @Nullable
    public static String run(@NotNull final String command) {
        try {
            BufferedReader reader = new BufferedReader(new InputStreamReader(
                    Runtime.getRuntime().exec(command).getInputStream()));
            StringBuilder builder = new StringBuilder();
            String aux;
            while ((aux = reader.readLine()) != null) {
                builder.append(aux);
            }
            return builder.toString();
        } catch (IOException e) {
            e.printStackTrace();
            return null;
        }
    }

    /**
     * Tries to get the absolute path for a command in the PATH.
     */
    @Nullable
    public static String locateExecutable(@NotNull final String command) {
        return run((SystemInfo.isWindows ? "where" : "which") + ' ' + command);
    }
}
