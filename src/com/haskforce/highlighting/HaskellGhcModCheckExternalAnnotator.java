package com.haskforce.highlighting;

import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class HaskellGhcModCheckExternalAnnotator extends ExternalAnnotator<PsiFile, GhcMod.Problems> {
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
                holder.createErrorAnnotation(problemRange, problem.message);
            } else {
                holder.createWeakWarningAnnotation(problemRange, problem.message.substring("Warning: ".length()));
            }
        }
    }
}
