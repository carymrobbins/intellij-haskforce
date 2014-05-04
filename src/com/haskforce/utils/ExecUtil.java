package com.haskforce.utils;

import com.intellij.openapi.util.SystemInfo;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class ExecUtil {
    @Nullable
    public static String exec(@NotNull final String command) {
        try {
            String[] commands;
            if (SystemInfo.isWindows) {
                commands = new String[] {"cmd", "/c", command};
            } else {
                // Default to UNIX if not Windows.
                commands = new String[] {"sh", "-c", command};
            }
            BufferedReader reader = new BufferedReader(new InputStreamReader(
                    Runtime.getRuntime().exec(commands).getInputStream()));
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
}
