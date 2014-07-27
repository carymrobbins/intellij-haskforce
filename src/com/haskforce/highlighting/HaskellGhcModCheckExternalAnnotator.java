package com.haskforce.highlighting;

import com.haskforce.HaskellFileType;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ModuleRootManager;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

public class HaskellGhcModCheckExternalAnnotator extends ExternalAnnotator<PsiFile, List<GhcMod.Problem>> {
    @Nullable
    @Override
    public PsiFile collectInformation(@NotNull PsiFile file) {
        return file;
    }

    @Nullable
    @Override
    public List<GhcMod.Problem> doAnnotate(PsiFile file) {
        final VirtualFile vFile = file.getVirtualFile();
        if (vFile == null || vFile.getFileType() != HaskellFileType.INSTANCE) {
            return null;
        }
        final String canonicalPath = vFile.getCanonicalPath();
        if (canonicalPath == null) {
            return null;
        }
        final Module module = ModuleUtilCore.findModuleForPsiElement(file);
        if (module == null) {
            return null;
        }
        final Sdk sdk = ModuleRootManager.getInstance(module).getSdk();
        if (sdk == null) {
            return null;
        }
        final String homePath = sdk.getHomePath();
        if (homePath == null) {
            return null;
        }
        final String workingDir = file.getProject().getBasePath();
        // Force all files to save to ensure annotations are in sync with the file system.
        ApplicationManager.getApplication().invokeAndWait(new Runnable() {
            @Override
            public void run() {
                FileDocumentManager.getInstance().saveAllDocuments();
            }
        }, ModalityState.any());
        return GhcMod.check(workingDir, file.getVirtualFile().getCanonicalPath());
    }

    @Override
    public void apply(@NotNull PsiFile file, List<GhcMod.Problem> annotationResult, @NotNull AnnotationHolder holder) {
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
            if (problem.message.startsWith("Warning: ")) {
                holder.createWeakWarningAnnotation(problemRange, problem.message.substring("Warning: ".length()));
            } else {
                holder.createErrorAnnotation(problemRange, problem.message);
            }
        }
    }
}
