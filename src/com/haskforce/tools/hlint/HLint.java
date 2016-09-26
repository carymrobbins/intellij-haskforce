package com.haskforce.tools.hlint;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.haskforce.system.packages.BuildInfo;
import com.haskforce.system.packages.HPackage;
import com.haskforce.system.packages.HPackageManager;
import com.haskforce.system.integrations.highlighting.HaskellAnnotationHolder;
import com.haskforce.system.integrations.highlighting.HaskellProblem;
import com.haskforce.system.integrations.highlighting.Problems;
import com.haskforce.haskell.psi.HaskellFile;
import com.haskforce.system.settings.ToolKey;
import com.haskforce.system.ui.tools.HaskellToolsConsole;
import com.haskforce.system.utils.*;
import com.haskforce.system.utils.ExecUtil.ExecError;
import com.haskforce.tools.hlint.intentions.IgnoreHLint;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.ParametersList;
import com.intellij.lang.annotation.Annotation;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.Option;
import scala.collection.JavaConversions;
import scala.collection.JavaConversions$;
import scala.runtime.AbstractFunction1;
import scala.util.Either;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Encapsulation of HLint internals. Uses hlint --json for precise error regions.
 * Still functional with older hlints, but error regions are less precise.
 */
public class HLint {
    private static final Logger LOG = Logger.getInstance(HLint.class);
    private static final VersionTriple HLINT_MIN_VERSION_WITH_JSON_SUPPORT = new VersionTriple(1, 9, 1);
    private static final Gson gson = new GsonBuilder().create();

    @NotNull
    public static Problems lint(final @NotNull Project project, @NotNull String workingDirectory,
                                @NotNull String file, @NotNull HaskellFile haskellFile) {
        final HaskellToolsConsole toolConsole = HaskellToolsConsole.get(project);
        final String hlintPath = ToolKey.HLINT_KEY.getPath(project);
        final String hlintFlags = ToolKey.HLINT_KEY.getFlags(project);
        if (hlintPath == null) return new Problems();

        return parseProblems(toolConsole, workingDirectory, hlintPath, hlintFlags, file, haskellFile, project).fold(
            new AbstractFunction1<ExecError, Problems>() {
                @Override
                public Problems apply(ExecError e) {
                    toolConsole.writeError(ToolKey.HLINT_KEY, e.getMessage());
                    NotificationUtil.displayToolsNotification(
                        NotificationType.ERROR, project, "hlint", e.getMessage()
                    );
                    return new Problems();
                }
            },
            FunctionUtil.<Problems>identity()
        );
    }

    @NotNull
    public static Either<ExecError, Problems> parseProblems(final HaskellToolsConsole toolConsole,
                                                            final @NotNull String workingDirectory,
                                                            final @NotNull String path,
                                                            final @NotNull String flags,
                                                            final @NotNull String file,
                                                            final @NotNull HaskellFile haskellFile,
                                                            final @NotNull Project project) {
        return getVersion(toolConsole, workingDirectory, path).fold(
            new AbstractFunction1<ExecError, Either<ExecError, Problems>>() {
                @Override
                public Either<ExecError, Problems> apply(ExecError e) {
                    return e.toLeft();
                }
            },
            new AbstractFunction1<VersionTriple, Either<ExecError, Problems>>() {
                @Override
                public Either<ExecError, Problems> apply(VersionTriple version) {
                    final boolean useJson = version.gte(HLINT_MIN_VERSION_WITH_JSON_SUPPORT);
                    final String[] params = getParams(file, haskellFile, useJson, project);
                    return EitherUtil.rightMap(runHlint(
                      toolConsole, workingDirectory, path, flags, params
                    ), new AbstractFunction1<String, Problems>() {
                        @Override
                        public Problems apply(String stdout) {
                            toolConsole.writeOutput(ToolKey.HLINT_KEY, stdout);
                            if (useJson) return parseProblemsJson(stdout);
                            return parseProblemsFallback(stdout);
                        }
                    });
                }
            }
        );
    }

    private static String[] getParams(@NotNull String file,
                                      @NotNull HaskellFile haskellFile,
                                      boolean useJson,
                                      @NotNull Project project) {
        final List<String> result = new ArrayList<>(1);
        result.add(file);
        if (useJson) result.add("--json");
        result.addAll(getParamsFromPackage(haskellFile, project));
        return result.toArray(new String[0]);
    }

    private static List<String> getParamsFromPackage(@NotNull HaskellFile haskellFile, @NotNull Project project) {
        HPackageManager packageManager = project.getComponent(HPackageManager.class);
        Option<HPackage> hPackage = packageManager.getPackageForFile(haskellFile.getVirtualFile());
        if (hPackage.isEmpty()) {
            LOG.warn("unable to find corresponding package for: " + haskellFile.getVirtualFile().getCanonicalPath());
            return new ArrayList<>();
        } else {
            return Optional.ofNullable(haskellFile.getVirtualFile())
                    .map(VirtualFile::getCanonicalPath)
                    .map(path -> hPackage.get().getBestMatchingBuildInfo(path))
                    .map(BuildInfo::getExtensions)
                    .map(set -> JavaConversions.seqAsJavaList(set.toList()))
                    .orElseGet(() -> new ArrayList<String>());
        }
    }

    /**
     * Parse problems from the hlint --json output.
     */
    @NotNull
    public static Problems parseProblemsJson(@NotNull String stdout) {
        final Problem[] problems = gson.fromJson(stdout, Problem[].class);
        if (problems == null) {
            LOG.warn("Unable to parse hlint json output: " + stdout);
            return new Problems();
        }
        return new Problems(problems);
    }

    /**
     * Parse a single problem from the old hlint output if json is not supported.
     */
    @Nullable
    public static Problem parseProblemFallback(String lint) {
        List<String> split = StringUtil.split(lint, ":");
        if (split.size() < 5) {
            return null;
        }
        int line = StringUtil.parseInt(split.get(1), 0);
        if (line == 0) {
            return null;
        }
        int column = StringUtil.parseInt(split.get(2), 0);
        if (column == 0) {
            return null;
        }
        String hint = StringUtil.split(split.get(4), "\n").get(0);
        split = StringUtil.split(lint, "\n");
        split = ContainerUtil.subList(split, 2);
        split = StringUtil.split(StringUtil.join(split, "\n"), "Why not:");
        if (split.size() != 2) {
            return null;
        }
        final String from = split.get(0).trim();
        final String to = split.get(1).trim();
        return new Problem("", "", hint, from, to, "", new String[]{}, "", line, column);
    }

    /**
     * Parse problems from the old hlint output if json is not supported.
     */
    @NotNull
    public static Problems parseProblemsFallback(String stdout) {
        final List<String> lints = StringUtil.split(stdout, "\n\n");
        Problems problems = new Problems();
        for (String lint : lints) {
            ContainerUtil.addIfNotNull(problems, parseProblemFallback(lint));
        }
        return problems;
    }

    private static final Pattern HLINT_VERSION_REGEX = Pattern.compile("(\\d+)\\.(\\d+)\\.(\\d+)");

    @NotNull
    private static Either<ExecError, VersionTriple> getVersion(HaskellToolsConsole toolConsole, String workingDirectory, String hlintPath) {
        return EitherUtil.rightFlatMap(
            runHlint(toolConsole, workingDirectory, hlintPath, "--version"),
            new AbstractFunction1<String, Either<ExecError, VersionTriple>>() {
                @Override
                public Either<ExecError, VersionTriple> apply(String version) {
                    Matcher m = HLINT_VERSION_REGEX.matcher(version);
                    if (!m.find()) {
                        return new ExecError(
                            "Could not parse version from hlint: '" + version + "'",
                            null
                        ).toLeft();
                    }
                    return EitherUtil.right(new VersionTriple(
                        Integer.parseInt(m.group(1)),
                        Integer.parseInt(m.group(2)),
                        Integer.parseInt(m.group(3))
                    ));
                }
            }
        );
    }

    /**
     * Runs hlintProg with parameters if hlintProg can be executed.
     */
    @NotNull
    private static Either<ExecError, String> runHlint(HaskellToolsConsole toolConsole,
                                                      @NotNull String workingDirectory,
                                                      @NotNull String hlintProg,
                                                      @NotNull String hlintFlags,
                                                      @NotNull String... params) {
        GeneralCommandLine commandLine = new GeneralCommandLine();
        commandLine.setWorkDirectory(workingDirectory);
        commandLine.setExePath(hlintProg);
        ParametersList parametersList = commandLine.getParametersList();
        // Required so that hlint won't report a non-zero exit status for lint issues.
        // Otherwise, ExecUtil.readCommandLine will return an error.
        parametersList.add("--no-exit-code");
        parametersList.addParametersString(hlintFlags);
        parametersList.addAll(params);
        toolConsole.writeInput(ToolKey.HLINT_KEY, "Using working directory: " + workingDirectory);
        toolConsole.writeInput(ToolKey.HLINT_KEY, commandLine.getCommandLineString());
        return ExecUtil.readCommandLine(commandLine);
    }

    public static class Problem extends HaskellProblem {
        public String decl;
        public String file;
        public String hint;
        public String from;
        public String to;
        public String module;
        public String[] note;
        public String severity;
        public int endLine;
        public int endColumn;
        public boolean useJson;

        /**
         * Provide a default constructor so gson objects will default to `useJson = true`.
         */
        public Problem() {
            useJson = true;
        }

        public Problem(String decl, String file, String hint, String from, String to, String module, String[] note,
                       String severity, int startLine, int startColumn, int endLine, int endColumn) {
            this.decl = decl;
            this.file = file;
            this.from = from;
            this.hint = hint;
            this.module = module;
            this.note = note;
            this.severity = severity;
            this.startLine = startLine;
            this.startColumn = startColumn;
            this.endLine = endLine;
            this.endColumn = endColumn;
            this.to = to;
        }

        public Problem(String decl, String file, String hint, String from, String to, String module, String[] note,
                       String severity, int startLine, int startColumn) {
            this(decl, file, hint, from, to, module, note, severity, startLine, startColumn, -1, -1);
            useJson = false;
        }

        public String getMessage() {
            return hint + (to == null || to.isEmpty() ? "" : ", why not: " + to);
        }

        protected void createAnnotation(@NotNull HaskellAnnotationHolder holder, int start, int end, @NotNull String message) {
            Annotation ann = holder.createWarningAnnotation(TextRange.create(start, end), message);
            if (ann != null) ann.registerFix(new IgnoreHLint(hint));
        }

        @Override
        public void createAnnotations(@NotNull PsiFile file, @NotNull HaskellAnnotationHolder holder) {
            final String text = file.getText();
            final int start = getOffsetStart(text);
            final int end = getOffsetEnd(start, text);
            if (start == -1 || end == -1) {
                return;
            }
            if (useJson && hint.equals("Use camelCase")) {
                createUseCamelCaseAnnotations(text, start, end, holder);
            } else {
                createDefaultAnnotations(start, end, holder);
            }
        }

        public static final Pattern NON_CAMEL_CASE_REGEX = Pattern.compile("\\b\\w+_\\w+\\b");

        public void createUseCamelCaseAnnotations(@NotNull String text, int start, int end, @NotNull HaskellAnnotationHolder holder) {
            final String section = text.substring(start, end);
            Matcher m = NON_CAMEL_CASE_REGEX.matcher(section);
            if (m.find()) {
                do {
                    createAnnotation(holder, start + m.start(), start + m.end(), "Use camelCase");
                } while (m.find());
            } else {
                createDefaultAnnotations(start, end, holder);
            }
        }

        public void createDefaultAnnotations(int start, int end, @NotNull HaskellAnnotationHolder holder) {
            createAnnotation(holder, start, end, getMessage());
        }

        public int getOffsetEnd(int offsetStart, String fileText) {
            final int offsetEnd = useJson ? StringUtil.lineColToOffset(fileText, endLine - 1, endColumn - 1)
                                          : getOffsetEndFallback(offsetStart, fileText);
            return offsetEnd == -1 ? fileText.length() : offsetEnd;
        }

        private static final Pattern WHITESPACE_REGEX = Pattern.compile("\\s+");

        /**
         * Fallback to a crude guess if the json output is not available from hlint.
         */
        public int getOffsetEndFallback(int offsetStart, String fileText) {
            int width = 0;
            int nonWhiteSpaceToFind = WHITESPACE_REGEX.matcher(from).replaceAll("").length();
            int nonWhiteSpaceFound = 0;
            while (offsetStart + width < fileText.length()) {
                final char c = fileText.charAt(offsetStart + width);
                if (StringUtil.isLineBreak(c)) {
                    break;
                }
                if (!StringUtil.isWhiteSpace(c)) {
                    ++nonWhiteSpaceFound;
                }
                ++width;
                if (nonWhiteSpaceFound >= nonWhiteSpaceToFind) {
                    break;
                }
            }
            return offsetStart + width;
        }
    }

    // TODO: VersionTriple may be useful in a util module or there may be an better existing implementation.
    public static class VersionTriple {
        private final int x;
        private final int y;
        private final int z;

        VersionTriple(final int x_, final int y_, final int z_) {
            x = x_;
            y = y_;
            z = z_;
        }

        public boolean eq(VersionTriple v) {
            return v != null && x == v.x && y == v.y && z == v.z;
        }

        public boolean gt(VersionTriple v) {
            return v != null && (x > v.x || x == v.x && (y > v.y || y == v.y && z > v.z));
        }

        public boolean lt(VersionTriple v) {
            return v != null && (x < v.x || x == v.x && (y < v.y || y == v.y && z < v.z));
        }

        public boolean gte(VersionTriple v) {
            return eq(v) || gt(v);
        }

        public boolean lte(VersionTriple v) {
            return eq(v) || lt(v);
        }
    }
}
