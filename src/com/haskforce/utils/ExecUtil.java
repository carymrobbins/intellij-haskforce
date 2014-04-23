package com.haskforce.utils;

import org.jetbrains.annotations.NotNull;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;

public class ExecUtil {
    public static String exec(@NotNull final String command) {
        try {
            // UNIX only, TODO: Windows
            String[] commands = {"sh", "-c", command};
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
