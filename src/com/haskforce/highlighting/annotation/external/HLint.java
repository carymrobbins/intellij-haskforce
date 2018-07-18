package com.haskforce.highlighting.annotation.external;

import com.google.gson.*;
import com.haskforce.cabal.query.BuildInfoUtil;
import com.haskforce.features.intentions.IgnoreHLint;
import com.haskforce.highlighting.annotation.HaskellAnnotationHolder;
import com.haskforce.highlighting.annotation.HaskellProblem;
import com.haskforce.highlighting.annotation.Problems;
import com.haskforce.psi.HaskellFile;
import com.haskforce.settings.ToolKey;
import com.haskforce.ui.tools.HaskellToolsConsole;
import com.haskforce.utils.*;
import com.haskforce.utils.ExecUtil.ExecError;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.configurations.ParametersList;
import com.intellij.lang.annotation.Annotation;
import com.intellij.notification.NotificationType;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.util.Either;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Encapsulation of HLint internals. Uses hlint --json for precise error regions.
 * Still functional with older hlints, but error regions are less precise.
 */
public class HLint {
    private static final Logger LOG = Logger.getInstance(HLint.class);
    private static final VersionTriple HLINT_MIN_VERSION_WITH_JSON_SUPPORT = new VersionTriple(1, 9, 1);
    private static final Gson gson =
        new GsonBuilder()
            .registerTypeAdapter(HLint.Problem.class, new HLintProblemGsonAdapter())
            .create();

    @NotNull
    public static Problems lint(final @NotNull Project project, @NotNull String workingDirectory,
                                @NotNull String file, @NotNull HaskellFile haskellFile) {
        final HaskellToolsConsole toolConsole = HaskellToolsConsole.get(project);
        final String hlintPath = ToolKey.HLINT_KEY.getPath(project);
        final String hlintFlags = ToolKey.HLINT_KEY.getFlags(project);
        if (hlintPath == null) return new Problems();

        return EitherUtil.valueOr(
            parseProblems(toolConsole, workingDirectory, hlintPath, hlintFlags, file, haskellFile),
            e -> {
                toolConsole.writeError(ToolKey.HLINT_KEY, e.getMessage());
                NotificationUtil.displayToolsNotification(
                  NotificationType.ERROR, project, "hlint", e.getMessage()
                );
                return new Problems();
            }
        );
    }

    @NotNull
    public static Either<ExecError, Problems> parseProblems(final HaskellToolsConsole toolConsole,
                                                            final @NotNull String workingDirectory,
                                                            final @NotNull String path,
                                                            final @NotNull String flags,
                                                            final @NotNull String file,
                                                            final @NotNull HaskellFile haskellFile) {
        return getVersion(toolConsole, workingDirectory, path).flatMap(version -> {
            final boolean useJson = version.$greater$eq(HLINT_MIN_VERSION_WITH_JSON_SUPPORT);
            final String[] params = getParams(file, haskellFile, useJson);
            return runHlint(toolConsole, workingDirectory, path, flags, params).map(stdout -> {
                toolConsole.writeOutput(ToolKey.HLINT_KEY, stdout);
                if (useJson) return parseProblemsJson(toolConsole, stdout);
                return parseProblemsFallback(stdout);
            });
        });
    }

    private static String[] getParams(@NotNull String file,
                                      @NotNull HaskellFile haskellFile,
                                      boolean useJson) {
        final List<String> result = new ArrayList<>(1);
        result.add(file);
        if (useJson) result.add("--json");
        result.addAll(getParamsFromCabal(haskellFile));
        return result.toArray(new String[0]);
    }

    private static List<String> getParamsFromCabal(@NotNull HaskellFile haskellFile) {
        return BuildInfoUtil.getExtensionOpts(haskellFile).run(
                ApplicationManager.getApplication(),
                IJReadActionRunner$.MODULE$.app()
        );
    }

    /**
     * Parse problems from the hlint --json output.
     */
    @NotNull
    public static Problems parseProblemsJson(@NotNull HaskellToolsConsole toolsConsole, @NotNull String stdout) {
        return EitherUtil.valueOr(
            parseProblemsJson(stdout),
            e -> {
                toolsConsole.writeError(ToolKey.HLINT_KEY, e.getMessage());
                LOG.error(e);
                return new Problems();
            }
        );
    }

    @NotNull
    public static Either<Throwable, Problems> parseProblemsJson(@NotNull String stdout) {
        final Problem[] problems;
        try {
            problems = gson.fromJson(stdout, Problem[].class);
        } catch (JsonSyntaxException e) {
            return EitherUtil.left(
              new IllegalStateException(
                "Unable to decode problem from HLint json output:\n"
                  + e.getMessage()
                  + "\nJSON was: " + stdout,
                e
              )
            );
        }
        if (problems == null) {
            return EitherUtil.left(
              new IllegalStateException(
                "Unable to decode problem from HLint json output, decoded as null; json was: " + stdout
              )
            );
        }
        return EitherUtil.right(new Problems(problems));
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
        return Problem.forFallback("", "", hint, from, to, "", new String[]{}, "", line, column);
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

    @NotNull
    private static Either<ExecError, VersionTriple> getVersion(HaskellToolsConsole toolConsole, String workingDirectory, String hlintPath) {
        return HLintUtil.runHLintNumericVersion(toolConsole, workingDirectory, hlintPath);
    }

    /**
     * Runs hlintProg with parameters if hlintProg can be executed.
     */
    @NotNull
    public static Either<ExecError, String> runHlint(HaskellToolsConsole toolConsole,
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

    public static class Problem implements HaskellProblem {

        public final String decl;
        public final String file;
        public final String hint;
        public final String from;
        public final String to;
        public final String module;
        public final String[] note;
        public final String severity;
        public final int startLine;
        public final int startColumn;
        public final int endLine;
        public final int endColumn;
        public final boolean useJson;

        public Problem(String decl, String file, String hint, String from, String to, String module, String[] note, String severity, int startLine, int startColumn, int endLine, int endColumn, boolean useJson) {
            this.decl = decl;
            this.file = file;
            this.hint = hint;
            this.from = from;
            this.to = to;
            this.module = module;
            this.note = note;
            this.severity = severity;
            this.startLine = startLine;
            this.startColumn = startColumn;
            this.endLine = endLine;
            this.endColumn = endColumn;
            this.useJson = useJson;
        }

        public static Problem forFallback(String decl, String file, String hint, String from, String to, String module, String[] note, String severity, int startLine, int startColumn) {
            return new Problem(decl, file, hint, from, to, module, note, severity, startLine, startColumn, -1, -1, false);
        }

        public String getMessage() {
            return hint + (to == null || to.isEmpty() ? "" : ", why not: " + to);
        }

        protected void createAnnotation(@NotNull HaskellAnnotationHolder holder, int start, int end, @NotNull String message) {
            Annotation ann = holder.createWarningAnnotation(TextRange.create(start, end), message);
            if (ann != null) ann.registerFix(new IgnoreHLint(hint));
        }

        @Override
        public int getStartLine() {
            return startLine;
        }

        @Override
        public int getStartColumn() {
            return startColumn;
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

        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;
            Problem problem = (Problem) o;
            return startLine == problem.startLine &&
              startColumn == problem.startColumn &&
              endLine == problem.endLine &&
              endColumn == problem.endColumn &&
              useJson == problem.useJson &&
              Objects.equals(decl, problem.decl) &&
              Objects.equals(file, problem.file) &&
              Objects.equals(hint, problem.hint) &&
              Objects.equals(from, problem.from) &&
              Objects.equals(to, problem.to) &&
              Objects.equals(module, problem.module) &&
              Arrays.equals(note, problem.note) &&
              Objects.equals(severity, problem.severity);
        }

        @Override
        public int hashCode() {
            int result = Objects.hash(decl, file, hint, from, to, module, severity, startLine, startColumn, endLine, endColumn, useJson);
            result = 31 * result + Arrays.hashCode(note);
            return result;
        }

        @Override
        public String toString() {
            return "Problem{" +
              "decl='" + decl + '\'' +
              ", file='" + file + '\'' +
              ", hint='" + hint + '\'' +
              ", from='" + from + '\'' +
              ", to='" + to + '\'' +
              ", module='" + module + '\'' +
              ", note=" + Arrays.toString(note) +
              ", severity='" + severity + '\'' +
              ", startLine=" + startLine +
              ", startColumn=" + startColumn +
              ", endLine=" + endLine +
              ", endColumn=" + endColumn +
              ", useJson=" + useJson +
              '}';
        }
    }

    /**
     * Custom deserialization to deal with the fact that hlint < 2.1.5 used String for the
     * 'module' and 'decl' fields whereas hlint >= 2.1.5 started using Array[String].
     * See https://github.com/ndmitchell/hlint/commit/7bb10df6871759704a287fedea623f5142ad2154#diff-c1ba1f9735f9cb9bfe8be220568d211b
     */
    private static class HLintProblemGsonAdapter implements JsonDeserializer<Problem> {

        @Override
        public Problem deserialize(JsonElement jsonElement, Type type, JsonDeserializationContext jsonDeserializationContext) throws JsonParseException {
            JsonObject o = jsonElement.getAsJsonObject();
            return new Problem(
                extractStringOrSingletonArray(o, "decl"),
                o.get("file").getAsString(),
                o.get("hint").getAsString(),
                o.get("from").getAsString(),
                extractOptional(o, "to", JsonElement::getAsString, () -> ""),
                extractStringOrSingletonArray(o, "module"),
                extractStringArray(o, "note"),
                o.get("severity").getAsString(),
                o.get("startLine").getAsInt(),
                o.get("startColumn").getAsInt(),
                o.get("endLine").getAsInt(),
                o.get("endColumn").getAsInt(),
                true
            );
        }

        private <A> A extractOptional(JsonObject o, String field, Function<JsonElement, A> ifNotNull, Supplier<A> ifNull) {
          if (!o.has(field)) return ifNull.get();
          JsonElement v = o.get(field);
          if (v.isJsonNull()) return ifNull.get();
          return ifNotNull.apply(v);
        }

        private String extractStringOrSingletonArray(JsonObject o, String field) {
            JsonElement v = o.get(field);
            if (v.isJsonArray()) {
                JsonArray a = v.getAsJsonArray();
                if (a.size() == 0) return "";
                if (a.size() != 1 || !a.get(0).isJsonPrimitive() || !a.get(0).getAsJsonPrimitive().isString()) {
                    throw new IllegalStateException("Expected singleton array of string at field '" + field + "'; found: " + v.toString());
                }
                return a.get(0).getAsJsonPrimitive().getAsString();
            }
            if (v.isJsonPrimitive() && v.getAsJsonPrimitive().isString()) {
                return v.getAsJsonPrimitive().getAsString();
            }
            throw new IllegalStateException("Expected either a string or array of string at field '" + field + "'; found: " + v.toString());
        }

        private String[] extractStringArray(JsonObject o, String field) {
            JsonElement v = o.get(field);
            if (!v.isJsonArray()) {
                throw new IllegalStateException("Expected string array at field '" + field + "'; got: " + v.toString());
            }
            JsonArray a = v.getAsJsonArray();
            String[] result = new String[a.size()];
            for (int i = 0; i < a.size(); ++i) {
                JsonElement x = a.get(i);
                if (!x.isJsonPrimitive() || !x.getAsJsonPrimitive().isString()) {
                    throw new IllegalStateException("At field '" + field + "', expected array index " + i + " to be a string; got: " + x.toString());
                }
                result[i] = x.getAsJsonPrimitive().getAsString();
            }
            return result;
        }
    }
}
