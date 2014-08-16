package com.haskforce.highlighting.annotation.external;

import com.haskforce.highlighting.annotation.HaskellAnnotationHolder;
import com.haskforce.highlighting.annotation.HaskellProblem;
import com.haskforce.highlighting.annotation.Problems;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public class HaskellExternalAnnotator extends ExternalAnnotator<PsiFile, Problems> {
    private static final Logger LOG = Logger.getInstance(HaskellExternalAnnotator.class);

    /**
     * Wrapper to simplify adding annotations to an AnnotationHolder.
     */
    @Override
    public void apply(@NotNull PsiFile file, Problems annotationResult, @NotNull AnnotationHolder holder) {
        apply(file, annotationResult, new HaskellAnnotationHolder(holder));
    }

    @NotNull
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

        // Use the module directory as working directory if it exists.
        // If not, use the project directory.
        final Project project = file.getProject();
        final Module module = ModuleUtilCore.findModuleForFile(file.getVirtualFile(), project);
        final VirtualFile moduleFile = module == null ? null : module.getModuleFile();
        final VirtualFile moduleDir = moduleFile == null ? null : moduleFile.getParent();
        final String workDir = moduleDir == null ? project.getBasePath() : moduleDir.getPath();
        Problems problems = new Problems();
        problems.addAllNotNull(GhcMod.check(project, workDir, canonicalPath));
        problems.addAllNotNull(HLint.lint(project, workDir, canonicalPath));
        return problems;
    }

    public static void apply(PsiFile file, Problems problems, HaskellAnnotationHolder holder) {
        if (problems == null || problems.isEmpty() || !file.isValid()) {
            return;
        }
        for (HaskellProblem problem : problems) {
            problem.createAnnotations(file, holder);
        }
    }

}
