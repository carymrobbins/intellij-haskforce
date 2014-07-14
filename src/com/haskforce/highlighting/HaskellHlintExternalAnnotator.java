/*
 * Copyright 2012-2013 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * Adapted from https://github.com/ignatov/intellij-erlang
 * src/org/intellij/erlang/dialyzer/ErlangDialyzerExternalAnnotator.java
 */

package com.haskforce.highlighting;

import com.haskforce.HaskellFileType;
import com.haskforce.utils.ExecUtil;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;
import java.util.regex.Pattern;

/**
 * An external annotator providing warning highlights from hlint.
 *
 * You can set the path to hlint via the Haskell Tools section of the project settings.  However, if it is
 * not set then the annotator will attempt to find hlint automatically and use it.  If it cannot find hlint it will
 * not attempt to provide annotations.
 *
 * The annotator runs once all other background processes have completed, such as the lexer, parser, highlighter, etc.
 * If the file does not parse, these annotations will not be available.
 */
public class HaskellHlintExternalAnnotator extends ExternalAnnotator<HaskellHlintExternalAnnotator.State, HaskellHlintExternalAnnotator.State> {
    private static final Logger LOG = Logger.getInstance(HaskellHlintExternalAnnotator.class);
    private static final Pattern WHITESPACE_REGEX = Pattern.compile("\\s+");

    private static Problem parseProblem(String input) {
        List<String> split = StringUtil.split(input, ":");
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
        String warning = StringUtil.split(split.get(4), "\n").get(0);
        split = StringUtil.split(input, "\n");
        split = ContainerUtil.subList(split, 2);
        split = StringUtil.split(StringUtil.join(split, "\n"), "Why not:");
        if (split.size() != 2) {
            return null;
        }
        final String found = split.get(0).trim();
        final String whyNot = split.get(1).trim();
        return new Problem(line, column, warning, found, whyNot);
    }

    @Nullable
    @Override
    public State collectInformation(@NotNull PsiFile file) {
        VirtualFile vFile = file.getVirtualFile();
        if (vFile == null || vFile.getFileType() != HaskellFileType.INSTANCE) {
            return null;
        }
        String canonicalPath = vFile.getCanonicalPath();
        if (canonicalPath == null) {
            return null;
        }
        Module module = ModuleUtilCore.findModuleForPsiElement(file);
        if (module == null) {
            return null;
        }
        Sdk sdk = ModuleRootManager.getInstance(module).getSdk();
        if (sdk == null) {
            return null;
        }
        String homePath = sdk.getHomePath();
        if (homePath == null) {
            return null;
        }

        final String workingDir = file.getProject().getBasePath();
        final String hlintPath = PropertiesComponent.getInstance(module.getProject()).getValue("hlintPath");
        return hlintPath == null || hlintPath.isEmpty() ? null : new State(hlintPath, file.getText(), workingDir);
    }

    @Nullable
    @Override
    public State doAnnotate(State state) {
        if (state == null) {
            return null;
        }
        // TODO: Refactor command line into a helper method in ExecUtil.
        GeneralCommandLine commandLine = new GeneralCommandLine();
        commandLine.setWorkDirectory(state.myWorkingDir);
        commandLine.setExePath(state.myHlintPath);
        // Read from stdin.
        commandLine.addParameter("-");
        String stdout = ExecUtil.readCommandLine(commandLine, state.myFileText);
        if (stdout == null || stdout.isEmpty()) {
            return null;
        }
        final List<String> lints = StringUtil.split(stdout, "\n\n");
        for (String lint : lints) {
            ContainerUtil.addIfNotNull(state.problems, parseProblem(lint));
        }
        return state;
    }

    @Override
    public void apply(@NotNull PsiFile file, State annotationResult, @NotNull AnnotationHolder holder) {
        if (annotationResult == null || !file.isValid()) {
            return;
        }
        String text = file.getText();
        for (Problem problem : annotationResult.problems) {
            int offset = StringUtil.lineColToOffset(text, problem.myLine - 1, problem.myColumn - 1);
            if (offset == -1) {
                continue;
            }
            // Since we don't have an ending line/column we can count non-whitespace to determine the highlight width.
            int width = 0;
            int nonWhiteSpaceToFind = WHITESPACE_REGEX.matcher(problem.myFound).replaceAll("").length();
            int nonWhiteSpaceFound = 0;
            while (offset + width < text.length()) {
                if (!StringUtil.isWhiteSpace(text.charAt(offset + width))) {
                    ++nonWhiteSpaceFound;
                }
                ++width;
                if (nonWhiteSpaceFound >= nonWhiteSpaceToFind) {
                    break;
                }
            }
            TextRange problemRange = TextRange.create(offset, offset + width);
            String message = problem.myWarning + ", why not: " + problem.myWhyNot;
            holder.createWarningAnnotation(problemRange, message);
            // TODO: Add an inspection to fix.
        }
    }

    public static class State {
        public final List<Problem> problems = new ArrayList<Problem>();
        private final String myHlintPath;
        private final String myFileText;
        private final String myWorkingDir;

        public State(String hlintPath, String fileText, String workingDir) {
            myHlintPath = hlintPath;
            myFileText = fileText;
            myWorkingDir = workingDir;
        }
    }

    public static class Problem {
        private final int myLine;
        private final int myColumn;
        private final String myWarning;
        private final String myFound;
        private final String myWhyNot;

        public Problem(int line, int column, String warning, String found, String whyNot) {
            myLine = line;
            myColumn = column;
            myWarning = warning;
            myFound = found;
            myWhyNot = whyNot;
        }
    }
}
