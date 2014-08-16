package com.haskforce.highlighting.annotation.external;

import com.haskforce.features.intentions.AddLanguagePragma;
import com.haskforce.features.intentions.AddTypeSignature;
import com.haskforce.features.intentions.RemoveForall;
import com.haskforce.highlighting.annotation.HaskellAnnotationHolder;
import com.haskforce.highlighting.annotation.HaskellProblem;
import com.haskforce.highlighting.annotation.Problems;
import com.haskforce.utils.ExecUtil;
import com.intellij.lang.annotation.Annotation;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Scanner;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Interface + encapsulation of details concerning ghc-mod communication and annotation.
 */
public class GhcMod {
    @NotNull
    public static Problems check(@NotNull Project project, @NotNull String workingDirectory, @NotNull String file) {
        String ghcModPath = getPath(project);
        String stdout;
        if (ghcModPath == null || (stdout = ExecUtil.readCommandLine(workingDirectory, ghcModPath, "check", file)) == null) {
            return new Problems();
        }
        return parseProblems(new Scanner(stdout));
    }

    @Nullable
    public static String getPath(@NotNull Project project) {
        return ExecUtil.getExternalToolPath(project, ExecUtil.GHC_MOD_PATH_KEY);
    }

    @NotNull
    public static Problems parseProblems(Scanner scanner) {
        Problems result = new Problems();
        Problem problem;
        while ((problem = parseProblem(scanner)) != null) {
            result.add(problem);
        }
        return result;
    }

    private static final Pattern IN_A_STMT_REGEX = Pattern.compile("\nIn a stmt.*");
    private static final Pattern USE_V_REGEX = Pattern.compile("\nUse -v.*");

    @Nullable
    public static Problem parseProblem(Scanner scanner) {
        if (scanner == null) {
            return null;
        }
        scanner.useDelimiter(":");
        if (!scanner.hasNext()) {
            return null;
        }
        String file = scanner.next();
        if (!scanner.hasNext()) {
            return null;
        }
        if (!scanner.hasNextInt()) {
            // We're probably parsing something like C:\path\to\file.hs
            file += ':' + scanner.next();
            if (!scanner.hasNextInt()) {
                return null;
            }
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
        // Remove "In a stmt..." text and set newlines.
        String message = scanner.next().replace('\0', '\n');
        // Remove "In a stmt ..."
        message = IN_A_STMT_REGEX.matcher(message).replaceAll("");
        // Remove "Use -v ..."
        message = USE_V_REGEX.matcher(message).replaceAll("");
        // Remove newlines from filename.
        file = file.trim();
        return new Problem(file, startLine, startColumn, message);
    }

    public static class Problem extends HaskellProblem {
        public String file;
        public String message;
        public boolean isError;

        public Problem(String file, int startLine, int startColumn, String message) {
            this.file = file;
            this.startLine = startLine;
            this.startColumn = startColumn;
            this.message = message;
            this.isError = !message.startsWith("Warning: ");
            if (this.isError) {
                this.message = message;
            } else {
                this.message = message.substring("Warning: ".length());
            }
        }

        abstract static class RegisterFixHandler {
            abstract public void apply(Matcher matcher, Annotation annotation, Problem problem);
        }

        /**
         * Intentions are identified using regex against the message received from ghc-mod.
         * The first regex match wins; all others will be ignored.
         * The RegisterFixHandler is used as an anonymous class so you can determine which fix, or fixes, to register.
         */
        static final List<Pair<Pattern, RegisterFixHandler>> fixHandlers;
        static {
            fixHandlers = new ArrayList(Arrays.asList(
                    new Pair(Pattern.compile("^Top-level binding with no type signature"),
                            new RegisterFixHandler() {
                                @Override
                                public void apply(Matcher matcher, Annotation annotation, Problem problem) {
                                    annotation.registerFix(new AddTypeSignature(problem));
                                }
                            }),
                    new Pair(Pattern.compile("^Illegal symbol '.' in type"),
                            new RegisterFixHandler() {
                                @Override
                                public void apply(Matcher matcher, Annotation annotation, Problem problem) {
                                    annotation.registerFix(new AddLanguagePragma("RankNTypes"));
                                    annotation.registerFix(new RemoveForall(problem));
                                }
                            }),
                    new Pair(Pattern.compile(" -X([A-Z][A-Za-z0-9]+)"),
                            new RegisterFixHandler() {
                                @Override
                                public void apply(Matcher matcher, Annotation annotation, Problem problem) {
                                    annotation.registerFix(new AddLanguagePragma(matcher.group(1)));
                                }
                            })
            ));
        }

        public void registerFix(Annotation annotation) {
            for (Pair<Pattern, RegisterFixHandler> p : fixHandlers) {
                final Matcher matcher = p.first.matcher(message);
                if (matcher.find()) {
                    p.second.apply(matcher, annotation, this);
                    // Bail out on first match.
                    return;
                }
            }
        }

        public static final Pattern WHITESPACE_REGEX = Pattern.compile("\\s");

        /**
         * Create an annotation from this problem and add it to the annotation holder.
         */
        @Override
        public void createAnnotations(@NotNull PsiFile psiFile, @NotNull HaskellAnnotationHolder holder) {
            final String text = psiFile.getText();
            final int offsetStart = getOffsetStart(text);
            if (offsetStart == -1) {
                return;
            }
            // TODO: There is probably a better way to compare these two file paths.
            // The problem might not be ours; ignore this problem in that case.
            // Note that Windows paths end up with different slashes, so getPresentableUrl() normalizes them.
            final VirtualFile vFile = psiFile.getVirtualFile();
            if (!(file.equals(vFile.getCanonicalPath()) || file.equals(vFile.getPresentableUrl()))) {
                return;
            }
            // Since we don't have ending regions from ghc-mod, highlight until the first whitespace.
            Matcher matcher = WHITESPACE_REGEX.matcher(text.substring(offsetStart));
            final int offsetEnd = matcher.find() ? offsetStart + matcher.start() : text.length() - 1;
            final TextRange range = TextRange.create(offsetStart, offsetEnd);
            final Annotation annotation;
            if (isError) {
                annotation = holder.createErrorAnnotation(range, message);
            } else {
                annotation = holder.createWeakWarningAnnotation(range, message);
            }
            registerFix(annotation);
        }
    }
}
