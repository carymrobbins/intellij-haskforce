package com.haskforce.highlighting.annotation.external;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.haskforce.highlighting.annotation.HaskellAnnotationHolder;
import com.haskforce.highlighting.annotation.HaskellProblem;
import com.haskforce.highlighting.annotation.Problems;
import com.haskforce.utils.ExecUtil;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;
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
    public static Problems lint(@NotNull Project project, @NotNull String workingDirectory, @NotNull String file) {
        final String hlintPath = getPath(project);
        return parseProblems(workingDirectory, hlintPath, file);
    }

    public static String getPath(@NotNull Project project) {
        return ExecUtil.getExternalToolPath(project, ExecUtil.HLINT_PATH_KEY);
    }

    @NotNull
    public static Problems parseProblems(@NotNull String workingDirectory, @NotNull String hlintPath, @NotNull String file) {
        VersionTriple version = getVersion(workingDirectory, hlintPath);
        if (version == null) {
            return new Problems();
        }
        final boolean useJson = version.lte(HLINT_MIN_VERSION_WITH_JSON_SUPPORT);
        final String stdout = ExecUtil.readCommandLine(workingDirectory, hlintPath,
                                                       useJson ? new String[]{"--json", file} : new String[]{file});
        if (stdout == null) {
            LOG.warn("Unable to get output from hlint");
            return new Problems();
        }
        if (useJson) {
            return parseProblemsJson(stdout);
        }
        return parseProblemsFallback(stdout);
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

    @Nullable
    private static VersionTriple getVersion(String workingDirectory, String hlintPath) {
        final String version = ExecUtil.readCommandLine(workingDirectory, hlintPath, "--version");
        if (version == null) {
            return null;
        }
        Matcher m = HLINT_VERSION_REGEX.matcher(version);
        if (!m.find()) {
            return null;
        }
        return new VersionTriple(
                Integer.parseInt(m.group(1)),
                Integer.parseInt(m.group(2)),
                Integer.parseInt(m.group(3)));
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

        @Override
        public void createAnnotations(@NotNull PsiFile file, @NotNull HaskellAnnotationHolder holder) {
            final String text = file.getText();
            final int start = getOffsetStart(text);
            final int end = getOffsetEnd(start, text);
            if (end == -1) {
                return;
            }
            if (useJson && hint.equals("Use camelCase")) {
                createUseCamelCaseAnnotations(text, start, end, holder);
            } else {
                createDefaultAnnotations(start, end, holder);
            }
        }

        public static Pattern NON_CAMEL_CASE_REGEX = Pattern.compile("\\b\\w+_\\w+\\b");

        public void createUseCamelCaseAnnotations(@NotNull String text, int start, int end, @NotNull HaskellAnnotationHolder holder) {
            final String section = text.substring(start, end);
            Matcher m = NON_CAMEL_CASE_REGEX.matcher(section);
            if (m.find()) {
                do {
                    final TextRange range = TextRange.create(start + m.start(), start + m.end());
                    holder.createWarningAnnotation(range, "Use camelCase");
                } while (m.find());
            } else {
                createDefaultAnnotations(start, end, holder);
            }
        }

        public void createDefaultAnnotations(int start, int end, @NotNull HaskellAnnotationHolder holder) {
            holder.createWarningAnnotation(TextRange.create(start, end), getMessage());
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
