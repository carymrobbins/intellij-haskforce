package com.haskforce.highlighting;

import com.haskforce.features.intentions.AddLanguagePragma;
import com.haskforce.features.intentions.AddTypeSignature;
import com.haskforce.features.intentions.RemoveForall;
import com.haskforce.highlighting.GhcMod.*;
import com.intellij.lang.annotation.Annotation;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * An external annotator providing warning and error highlights from ghc-mod check.
 *
 * You can set the path to ghc-mod via the Haskell Tools section of the project settings.
 *
 * The annotator runs once all other background processes have completed, such as the lexer, parser, highlighter, etc.
 * If the file does not parse, these annotations will not be available.
 *
 * To add intentions, update the static initializer for fixHandlers below.
 */
public class HaskellGhcModCheckExternalAnnotator extends HaskellExternalAnnotatorBase<PsiFile, Problems> {
    @Nullable
    @Override
    public PsiFile collectInformation(@NotNull PsiFile file) {
        return file;
    }

    @Nullable
    @Override
    public Problems doAnnotate(PsiFile file) {
        final String canonicalPath = file.getVirtualFile().getCanonicalPath();
        if (canonicalPath == null) {
            return null;
        }

        // Force all files to save to ensure annotations are in sync with the file system.
        ApplicationManager.getApplication().invokeAndWait(new Runnable() {
            @Override
            public void run() {
                FileDocumentManager.getInstance().saveAllDocuments();
            }
        }, ModalityState.any());

        final Project project = file.getProject();
        return GhcMod.check(project, file.getProject().getBasePath(), canonicalPath);
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
                new Pair(Pattern.compile(" -X([A-Z][A-Za-z0-9]+) "),
                        new RegisterFixHandler() {
                            @Override
                            public void apply(Matcher matcher, Annotation annotation, Problem problem) {
                                annotation.registerFix(new AddLanguagePragma(matcher.group(1)));
                            }
                        })
        ));
    }

    static void registerFix(Annotation annotation, Problem problem) {
        for (Pair<Pattern, RegisterFixHandler> p : fixHandlers) {
            final Matcher matcher = p.first.matcher(problem.message);
            if (matcher.find()) {
                p.second.apply(matcher, annotation, problem);
                // Bail out on first match.
                return;
            }
        }
    }

    @Override
    public void apply(@NotNull PsiFile file, Problems annotationResult, @NotNull AnnotationHolder holder) {
        if (annotationResult == null || !file.isValid() || annotationResult.isEmpty()) {
            return;
        }
        String text = file.getText();
        for (Problem problem : annotationResult) {
            final int offsetStart = StringUtil.lineColToOffset(file.getText(), problem.startLine - 1, problem.startColumn - 1);
            if (offsetStart == -1) {
                continue;
            }

            // The problem might not be ours; move on to the next problem in
            // that case.
            if(!problem.file.equals(file.getVirtualFile().getCanonicalPath())) {
                continue;
            }

            int offsetEnd = offsetStart;
            while ((++offsetEnd) < text.length()) {
                final char c = text.charAt(offsetEnd);
                if (StringUtil.isWhiteSpace(c)) {
                    break;
                }
            }
            TextRange problemRange = TextRange.create(offsetStart, offsetEnd);
            final Annotation annotation;
            if (problem.isError) {
                 annotation = createErrorAnnotation(holder, problemRange, problem.message);
            } else {
                annotation = createWeakWarningAnnotation(holder, problemRange, problem.message);
            }
            registerFix(annotation, problem);
        }
    }
}
