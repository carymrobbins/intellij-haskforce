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
import com.intellij.execution.ExecutionException;
import com.intellij.execution.configurations.GeneralCommandLine;
import com.intellij.execution.process.CapturingProcessHandler;
import com.intellij.ide.util.PropertiesComponent;
import com.intellij.lang.annotation.Annotation;
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

import java.io.BufferedWriter;
import java.io.IOException;
import java.io.OutputStreamWriter;
import java.util.ArrayList;
import java.util.List;

public class HaskellHlintExternalAnnotator extends ExternalAnnotator<HaskellHlintExternalAnnotator.State, HaskellHlintExternalAnnotator.State> {
    private static final Logger LOG = Logger.getInstance(HaskellHlintExternalAnnotator.class);

    private static Problem parseProblem(String input) {
        List<String> split = StringUtil.split(input, ":");
        if (split.size() < 3) {
            return null;
        }
        int line = StringUtil.parseInt(split.get(1), 0);
        return new Problem(line, split.get(split.size() - 1).trim());
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

        String workingDir = file.getProject().getBasePath();
        // TODO: Add this to the external tools and pull from there.
        String hlintPath = PropertiesComponent.getInstance(module.getProject()).getValue("hlintPath");
        if (hlintPath == null) {
            hlintPath = ExecUtil.locateExecutableByGuessing("hlint");
        }
        return new State(hlintPath, file.getText(), workingDir);
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
            int offset = StringUtil.lineColToOffset(text, problem.myLine - 1, 0);
            if (offset == -1) {
                continue;
            }
            int width = 0;
            while (offset + width < text.length() && !StringUtil.isLineBreak(text.charAt(offset + width))) {
                ++width;
            }
            TextRange problemRange = TextRange.create(offset, offset + width);
            String message = "Why not: " + problem.myDescription;
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
        private final String myDescription;

        public Problem(int line, String description) {
            myLine = line;
            myDescription = description;
        }

        @Override
        public String toString() {
            return "Problem{myLine=" + myLine +
                         ", myDescription='" + myDescription + "'}";
        }
    }
}
