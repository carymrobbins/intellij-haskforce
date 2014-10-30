package com.haskforce.highlighting.annotation.external;

import com.haskforce.codeInsight.HaskellCompletionContributor;
import com.haskforce.highlighting.annotation.HaskellAnnotationHolder;
import com.haskforce.highlighting.annotation.HaskellProblem;
import com.haskforce.highlighting.annotation.Problems;
import com.haskforce.utils.ExecUtil;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.ExternalAnnotator;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.application.ModalityState;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Single annotator that calls all external tools used for annotations.
 */
public class HaskellExternalAnnotator extends ExternalAnnotator<PsiFile, Problems> {
    @SuppressWarnings("UnusedDeclaration")
    private static final Logger LOG = Logger.getInstance(HaskellExternalAnnotator.class);

    /**
     * The default implementation here is to not annotate files that have lexer/parser errors.  This is kind
     * of lame since the error may be invalid.
     */
    @Nullable @Override
    public PsiFile collectInformation(@NotNull PsiFile file, @NotNull Editor editor, boolean hasErrors) {
        return collectInformation(file);
    }

    @NotNull
    @Override
    public PsiFile collectInformation(@NotNull PsiFile file) {
        return file;
    }

    @Nullable
    @Override
    public Problems doAnnotate(@NotNull PsiFile file) {
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
        final String workDir = ExecUtil.guessWorkDir(file);
        HaskellCompletionContributor.loadCacheData(file);
        Problems problems = new Problems();
        if (ExecUtil.GHC_MODI_KEY.getPath(project) != null) {
            final Module module = ModuleUtilCore.findModuleForPsiElement(file);
            if (module != null) {
                GhcModi ghcModi = module.getComponent(GhcModi.class);
                if (ghcModi != null) {
                    problems.addAllNotNull(ghcModi.check(canonicalPath));
                }
            }
        } else {
            problems.addAllNotNull(GhcMod.check(project, workDir, canonicalPath));
        }
        problems.addAllNotNull(HLint.lint(project, workDir, canonicalPath));
        return problems;
    }

    /**
     * Wrapper to simplify adding annotations to an AnnotationHolder.
     */
    @Override
    public void apply(@NotNull PsiFile file, Problems annotationResult, @NotNull AnnotationHolder holder) {
        apply(file, annotationResult, new HaskellAnnotationHolder(holder));
    }

    public static void apply(@NotNull PsiFile file, Problems problems, @NotNull HaskellAnnotationHolder holder) {
        if (problems == null || problems.isEmpty() || !file.isValid()) {
            return;
        }
        for (HaskellProblem problem : problems) {
            problem.createAnnotations(file, holder);
        }
    }

}
