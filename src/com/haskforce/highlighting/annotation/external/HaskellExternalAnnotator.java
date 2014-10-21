package com.haskforce.highlighting.annotation.external;

import com.haskforce.codeInsight.HaskellCompletionContributor;
import com.haskforce.highlighting.annotation.HaskellAnnotationHolder;
import com.haskforce.highlighting.annotation.HaskellProblem;
import com.haskforce.highlighting.annotation.Problems;
import com.haskforce.utils.ExecUtil;
import com.haskforce.utils.LogicUtil;
import com.intellij.codeInsight.lookup.LookupElement;
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
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * Single annotator that calls all external tools used for annotations.
 */
public class HaskellExternalAnnotator extends ExternalAnnotator<PsiFile, Problems> {
    private static final Logger LOG = Logger.getInstance(HaskellExternalAnnotator.class);

    /**
     * The default implementation here is to not annotate files that have lexer/parser errors.  This is kind
     * of lame since the error may be invalid.
     */
    @Nullable
    @Override
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
        final Module module = ModuleUtilCore.findModuleForFile(file.getVirtualFile(), project);
        final VirtualFile moduleFile = module == null ? null : module.getModuleFile();
        final VirtualFile moduleDir = moduleFile == null ? null : moduleFile.getParent();
        final String workDir = moduleDir == null ? project.getBasePath() : moduleDir.getPath();
        loadCacheData(file, project, workDir);
        Problems problems = new Problems();
        if (ExecUtil.GhcModiToolKey.isEnabledFor(project)) {
            problems.addAllNotNull(GhcModi.check(project, workDir, canonicalPath));
        } else {
            problems.addAllNotNull(GhcMod.check(project, workDir, canonicalPath));
        }
        problems.addAllNotNull(HLint.lint(project, workDir, canonicalPath));
        return problems;
    }

    /**
     * Helper method to load data from ghc-mod into a file cache to be used for autocompletion.  This is done in a
     * Java thread so execution can continue.  It just so happens to be very convenient to do this in the external
     * annotator; however, there may be a better place to do this.
     */
    public static void loadCacheData(@NotNull final PsiFile file, @NotNull final Project project, @NotNull final String workDir) {
        ApplicationManager.getApplication().invokeLater(new Runnable() {
            @Override
            public void run() {
                file.putUserData(ExecUtil.LANGUAGE_CACHE_KEY, mapStringToLookupElement(GhcMod.lang(project, workDir)));
                file.putUserData(ExecUtil.FLAG_CACHE_KEY, GhcMod.flag(project, workDir));
                file.putUserData(ExecUtil.MODULE_CACHE_KEY, GhcMod.list(project, workDir));
            }
        });
    }

    public static List<LookupElement> mapStringToLookupElement(String[] strings) {
        return LogicUtil.map(HaskellCompletionContributor.stringToLookupElement, strings);
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
