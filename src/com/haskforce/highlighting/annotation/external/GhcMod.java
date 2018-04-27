package com.haskforce.highlighting.annotation.external;

import com.haskforce.features.intentions.AddLanguagePragma;
import com.haskforce.features.intentions.AddTypeSignature;
import com.haskforce.features.intentions.RemoveForall;
import com.haskforce.highlighting.annotation.HaskellAnnotationHolder;
import com.haskforce.highlighting.annotation.HaskellProblem;
import com.haskforce.highlighting.annotation.Problems;
import com.haskforce.highlighting.annotation.external.GhcModUtil.GhcVersionValidation;
import com.haskforce.psi.HaskellImpdecl;
import com.haskforce.settings.ToolKey;
import com.haskforce.ui.tools.HaskellToolsConsole;
import com.haskforce.utils.ExecUtil;
import com.haskforce.utils.NotificationUtil;
import com.haskforce.utils.EitherUtil;
import com.intellij.codeInspection.ProblemHighlightType;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.ParametersList;
import com.intellij.lang.annotation.Annotation;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.VisualPosition;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.util.Either;

import java.io.File;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Interface + encapsulation of details concerning ghc-mod communication and annotation.
 */
public class GhcMod {
    @SuppressWarnings("UnusedDeclaration")
    private static final Logger LOG = Logger.getInstance(GhcMod.class);

    // Map of module -> errorMessage.  Useful to ensure we don't output the same error multiple times.
    private static Map<Module, String> errorState = new HashMap<Module, String>(0);

    private static Map<Project, GhcVersionValidation> ghcVersionValidationMap =
        new HashMap<Project, GhcVersionValidation>(0);

    @Nullable
    public static String getPath(@NotNull Project project) {
        return GhcModUtil.changedPathIfStack(project, ToolKey.GHC_MOD_KEY.getPath(project));
    }

    @NotNull
    public static String getFlags(@NotNull Project project) {
        return GhcModUtil.changedFlagsIfStack(
          project,
          ToolKey.GHC_MOD_KEY.getPath(project),
          ToolKey.GHC_MOD_KEY.getFlags(project)
        );
    }

    @Nullable
    public static Problems check(@NotNull Module module, @NotNull String workingDirectory, @NotNull String file) {
        final String stdout = simpleExec(module, workingDirectory, getFlags(module.getProject()), "check", file);
        return stdout == null ? new Problems() : handleCheck(module, stdout);
    }

    @Nullable
    public static Problems handleCheck(@NotNull Module module, @NotNull String stdout) {
        final Problems problems = parseProblems(module, new Scanner(stdout));
        if (problems == null) {
            // parseProblems should have returned something, so let's just dump the output to the user.
            displayError(module, stdout);
            return null;
        } else if (problems.size() == 1) {
            final Problem problem = (Problem)problems.get(0);
            if (problem.startLine == 0 && problem.startColumn == 0) {
                displayError(module, problem.message);
                return null;
            }
        }
        // Clear the errorState since ghc-mod was successful.
        errorState.remove(module);
        return problems;
    }

    @Nullable
    public static String[] list(@NotNull Module module, @NotNull String workingDirectory) {
        return simpleExecToLines(module, workingDirectory, getFlags(module.getProject()), "list");
    }

    public static void displayError(@NotNull Module module, @NotNull String message) {
        if (!message.equals(errorState.get(module))) {
            errorState.put(module, message);
            NotificationUtil.displayToolsNotification(NotificationType.ERROR, module.getProject(), "ghc-mod error", message);
        }
    }

    @Nullable
    public static String simpleExec(@NotNull Module module, @NotNull String workingDirectory,
                                    @NotNull String ghcModFlags, @NotNull String command, String... params) {
        final String ghcModPath = getPath(module.getProject());
        final String stdout;
        if (ghcModPath == null
                || (stdout = exec(module.getProject(), workingDirectory, ghcModPath, command, ghcModFlags, params)) == null
                || stdout.length() == 0) {
            return null;
        }
        return stdout;
    }

    @Nullable
    public static String[] simpleExecToLines(@NotNull Module module, @NotNull String workingDirectory,
                                             @NotNull String ghcModFlags, @NotNull String command, String... params) {
        final String result = simpleExec(module, workingDirectory, ghcModFlags, command, params);
        return result == null ? null : StringUtil.splitByLines(result);
    }

    @Nullable
    public static String exec(@NotNull Project project, @NotNull String workingDirectory, @NotNull String ghcModPath,
                              @NotNull String command, @NotNull String ghcModFlags, String... params) {
        if (!validateGhcVersion(project, ghcModPath, ghcModFlags)) return null;
        GeneralCommandLine commandLine = new GeneralCommandLine(ghcModPath);
        GhcModUtil.updateEnvironment(project, commandLine.getEnvironment());
        ParametersList parametersList = commandLine.getParametersList();
        parametersList.addParametersString(ghcModFlags);
        parametersList.add(command);
        parametersList.addAll(params);
        // setWorkDirectory is deprecated but is needed to work with IntelliJ 13 which does not have withWorkDirectory.
        commandLine.setWorkDirectory(workingDirectory);
        // Make sure we can actually see the errors.
        commandLine.setRedirectErrorStream(true);
        HaskellToolsConsole toolConsole = HaskellToolsConsole.get(project);
        toolConsole.writeInput(ToolKey.GHC_MOD_KEY, "Using working directory: " + workingDirectory);
        toolConsole.writeInput(ToolKey.GHC_MOD_KEY, commandLine.getCommandLineString());
        Either<ExecUtil.ExecError, String> result = ExecUtil.readCommandLine(commandLine);
        if (result.isLeft()) {
            //noinspection ThrowableResultOfMethodCallIgnored
            ExecUtil.ExecError e = EitherUtil.unsafeGetLeft(result);
            toolConsole.writeError(ToolKey.GHC_MOD_KEY, e.getMessage());
            NotificationUtil.displayToolsNotification(
                NotificationType.ERROR, project, "ghc-mod", e.getMessage()
            );
            return null;
        }
        String out = EitherUtil.unsafeGetRight(result);
        toolConsole.writeOutput(ToolKey.GHC_MOD_KEY, out);
        return out;
    }

    private static boolean validateGhcVersion(@NotNull Project project,
                                              @NotNull String ghcModPath,
                                              @NotNull String ghcModFlags) {
        GhcVersionValidation v = ghcVersionValidationMap.get(project);
        GhcVersionValidation newV = GhcModUtil.validateGhcVersion(v, project, ghcModPath, ghcModFlags);
        ghcVersionValidationMap.put(project, newV);
        return newV == GhcVersionValidation.VALID;
    }

    /** Similar to parseProblems(Scanner), except also resolves absolute file paths. */
    @Nullable
    public static Problems parseProblems(@NotNull Module module, @NotNull Scanner scanner) {
        List<Problem> problems = parseProblems(scanner);
        Problems result = new Problems();
        if (problems == null) return null;
        for (Problem problem : problems) {
            String absPath = inferAbsolutePath(module, problem.file);
            if (absPath != null) {
                result.add(new Problem(absPath, problem.startLine, problem.startColumn, problem.message));
            } else {
                result.add(problem);
            }
        }
        return result;
    }

    /** Continually parses from scanner until end of input, returning a list of problems. */
    @Nullable
    public static List<Problem> parseProblems(@NotNull Scanner scanner) {
        List<Problem> result = new ArrayList<Problem>();
        Problem problem;
        while (scanner.hasNext()) {
            problem = parseProblem(scanner);
            if (problem != null) {
                result.add(problem);
            }
        }
        // We only call this function if ghc-mod returned errors, so if we couldn't parse a result something
        // bad happened.  We'll check for a null return value in handleCheck.
        return result.size() == 0 ? null : result;
    }

    private static final Pattern IN_A_STMT_REGEX = Pattern.compile("\nIn a stmt.*");
    private static final Pattern USE_V_REGEX = Pattern.compile("\nUse -v.*");

    /** Parses a single problem from the scanner. */
    public static Problem parseProblem(@NotNull Scanner scanner) {
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

    /** Infers the absolute path given a relative one and its enclosing module. */
    @Nullable
    private static String inferAbsolutePath(@NotNull Module module, @NotNull String path) {
        File file = new File(path);
        if (file.exists()) return file.getAbsolutePath();
        final VirtualFile moduleFile = module.getModuleFile();
        if (moduleFile == null) return null;
        final String inferredPath = FileUtil.join(moduleFile.getParent().getCanonicalPath(), path);
        if (new File(inferredPath).exists()) return inferredPath;
        return null;
    }

    @Nullable
    public static String type(@NotNull Module module, @NotNull String workDir, @NotNull String canonicalPath,
                              VisualPosition startPosition, @NotNull VisualPosition stopPosition) {
        final String stdout = simpleExec(module, workDir, getFlags(module.getProject()), "type" , canonicalPath,
                String.valueOf(startPosition.line), String.valueOf(startPosition.column));
        if (stdout == null) return "Type info not found";
        try {
            return GhcModUtil.handleTypeInfo(startPosition, stopPosition, stdout);
        } catch (GhcModUtil.TypeInfoParseException e) {
              NotificationUtil.displayToolsNotification(
                      NotificationType.ERROR, module.getProject(), "Type Info Error",
                      "There was an error when executing the `ghc-mod type` command:\n\n" + stdout);
              return null;
        }
    }

    public static class Problem extends HaskellProblem {
        public String file;
        public String message;
        public boolean isError;
        public boolean isUnknownSymbol;
        public boolean isUnusedSymbol;
        public boolean isUnusedImport;

        private static Pattern notInScopeRegex = Pattern.compile("(?i)not in scope");

        public Problem(String file, int startLine, int startColumn, String message) {
            this.file = file;
            this.startLine = startLine;
            this.startColumn = startColumn;
            this.message = message;
            this.isError = !message.startsWith("Warning: ");
            if (!this.isError) this.message = message.substring("Warning: ".length());
            isUnknownSymbol = notInScopeRegex.matcher(message).find();
            isUnusedSymbol = message.contains("Defined but not used");
            isUnusedImport = message.contains("import of") && message.contains("is redundant");
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
            fixHandlers = new ArrayList<>(Arrays.<Pair<Pattern, RegisterFixHandler>>asList(
                    Pair.create(Pattern.compile("^Top-level binding with no type signature"),
                            new RegisterFixHandler() {
                                @Override
                                public void apply(Matcher matcher, Annotation annotation, Problem problem) {
                                    annotation.registerFix(new AddTypeSignature(problem));
                                }
                            }),
                    Pair.create(Pattern.compile("^Illegal symbol '.' in type"),
                            new RegisterFixHandler() {
                                @Override
                                public void apply(Matcher matcher, Annotation annotation, Problem problem) {
                                    annotation.registerFix(new AddLanguagePragma("RankNTypes"));
                                    annotation.registerFix(new RemoveForall(problem));
                                }
                            }),
                    Pair.create(Pattern.compile(" -X([A-Z][A-Za-z0-9]+)"),
                            new RegisterFixHandler() {
                                @Override
                                public void apply(Matcher matcher, Annotation annotation, Problem problem) {
                                    annotation.registerFix(new AddLanguagePragma(matcher.group(1)));
                                }
                            })
            ));
        }

        public void registerFix(@NotNull Annotation annotation) {
            for (Pair<Pattern, RegisterFixHandler> p : fixHandlers) {
                final Matcher matcher = p.first.matcher(message);
                if (matcher.find()) {
                    p.second.apply(matcher, annotation, this);
                    // Bail out on first match.
                    return;
                }
            }
        }

        /**
         * Create an annotation from this problem and add it to the annotation holder.
         */
        @Override
        public void createAnnotations(@NotNull PsiFile psiFile, @NotNull HaskellAnnotationHolder holder) {
            // TODO: There is probably a better way to compare these two file paths.
            // The problem might not be ours; ignore this problem in that case.
            // Note that Windows paths end up with different slashes, so getPresentableUrl() normalizes them.
            final VirtualFile vFile = psiFile.getVirtualFile();
            if (!(file.equals(vFile.getCanonicalPath()) || file.equals(vFile.getPresentableUrl()))) {
                return;
            }
            final TextRange range = getTextRange(psiFile);
            if (range == null) return;
            final Annotation annotation;
            if (isError) {
                annotation = holder.createErrorAnnotation(range, message);
            } else {
                annotation = holder.createWeakWarningAnnotation(range, message);
            }
            if (annotation == null) return;
            if (isUnknownSymbol) {
                annotation.setHighlightType(ProblemHighlightType.LIKE_UNKNOWN_SYMBOL);
            } else if (isUnusedSymbol) {
                annotation.setHighlightType(ProblemHighlightType.LIKE_UNUSED_SYMBOL);
            } else if (isUnusedImport) {
                annotation.setHighlightType(ProblemHighlightType.LIKE_UNUSED_SYMBOL);
            }
            registerFix(annotation);
        }

        /** The text range of our annotation should be based on the element at that offset. */
        @Nullable
        private TextRange getTextRange(@NotNull PsiFile psiFile) {
            final String text = psiFile.getText();
            final int offsetStart = getOffsetStart(text);
            if (offsetStart == -1) return null;
            PsiElement el = PsiTreeUtil.findElementOfClassAtOffset(psiFile, offsetStart, PsiElement.class, false);
            if (el == null) return null;
            // It's prettier to show the entire import line as unused instead of just the `import` keyword.
            if (isUnusedImport && el.getParent() instanceof HaskellImpdecl) return el.getParent().getTextRange();
            return el.getTextRange();
        }
    }
}
