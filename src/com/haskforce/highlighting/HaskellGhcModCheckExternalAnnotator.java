package com.haskforce.highlighting;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * An external annotator providing warning and error highlights from ghc-mod check.
 *
 * You can set the path to ghc-mod via the Haskell Tools section of the project settings.
 *
 * The annotator runs once all other background processes have completed, such as the lexer, parser, highlighter, etc.
 * If the file does not parse, these annotations will not be available.
 */
public class HaskellGhcModCheckExternalAnnotator extends HaskellExternalAnnotatorBase<PsiFile, GhcMod.Problems> {
    @Nullable
    @Override
    public PsiFile collectInformation(@NotNull PsiFile file) {
        return file;
    }

    @Nullable
    @Override
    public GhcMod.Problems doAnnotate(PsiFile file) {
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

    @Override
    public void apply(@NotNull PsiFile file, GhcMod.Problems annotationResult, @NotNull AnnotationHolder holder) {
        if (annotationResult == null || !file.isValid() || annotationResult.isEmpty()) {
            return;
        }
        String text = file.getText();
        for (GhcMod.Problem problem : annotationResult) {
            final int offsetStart = StringUtil.lineColToOffset(file.getText(), problem.startLine - 1, problem.startColumn - 1);
            if (offsetStart == -1) {
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
            if (problem.isError) {
                createErrorAnnotation(holder, problemRange, problem.message);
            } else {
                createWeakWarningAnnotation(holder, problemRange, problem.message.substring("Warning: ".length()));
            }
        }
    }
}
