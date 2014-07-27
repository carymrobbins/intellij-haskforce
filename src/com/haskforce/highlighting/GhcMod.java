package com.haskforce.highlighting;

import com.haskforce.utils.ExecUtil;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.openapi.util.text.StringUtil;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Scanner;

public class GhcMod {
    @Nullable
    public static Problems check(String workingDirectory, String file) {
        // TODO: Add ghc-mod in Haskell Tools.
        GeneralCommandLine commandLine = new GeneralCommandLine("ghc-mod", "check", file);
        commandLine.setWorkDirectory(workingDirectory);
        final String stdout = ExecUtil.readCommandLine(commandLine);
        if (stdout == null) {
            return null;
        }
        return parseProblems(new Scanner(stdout));
    }

    public static Problems parseProblems(Scanner scanner) {
        Problems result = new Problems();
        Problem problem;
        while ((problem = parseProblem(scanner)) != null) {
            result.add(problem);
        }
        return result;
    }

    @Nullable
    public static Problem parseProblem(Scanner scanner) {
        scanner.useDelimiter(":");
        if (!scanner.hasNext()) {
            return null;
        }
        final String file = scanner.next();
        if (!scanner.hasNextInt()) {
            return null;
        }
        final int startLine = scanner.nextInt();
        if (!scanner.hasNextInt()) {
            return null;
        }
        final int startColumn = scanner.nextInt();
        scanner.skip(":");
        scanner.useDelimiter("\n");
        if (!scanner.hasNext()) {
            return null;
        }
        final String message = StringUtil.split(scanner.next(), "\0In a stmt").get(0).replace('\0', '\n');
        return new Problem(file, startLine, startColumn, message);
    }

    public static class Problems extends ArrayList<Problem>{}

    public static class Problem {
        public String file;
        public int startLine;
        public int startColumn;
        public String message;
        public boolean isError;

        public Problem(String file, int startLine, int startColumn, String message) {
            this.file = file;
            this.startLine = startLine;
            this.startColumn = startColumn;
            this.message = message;
            this.isError = !message.startsWith("Warning: ");
        }
    }
}
