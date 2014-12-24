package com.haskforce.highlighting.annotation;

import com.google.common.collect.Lists;
import com.haskforce.highlighting.annotation.external.GhcMod;
import com.haskforce.highlighting.annotation.external.GhcModi;
import com.haskforce.highlighting.annotation.external.HaskellExternalAnnotator;
import com.haskforce.settings.ToolKey;
import com.haskforce.utils.ExecUtil;
import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.LogicalPosition;
import com.intellij.openapi.editor.VisualPosition;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import org.jetbrains.annotations.Nullable;

import java.awt.*;
import java.util.List;
import java.util.concurrent.ExecutionException;

public class HaskellDocumentationProvider extends AbstractDocumentationProvider {
    @Nullable
    @Override
    public String getQuickNavigateInfo(PsiElement element, PsiElement originalElement) {
        return getTypeInfo(element);
    }

    private static String getTypeInfo(PsiElement element) {
        FileEditorManager fileEditorManager = FileEditorManager.getInstance(element.getProject());
        if (fileEditorManager == null){
            return null;
        }
        /*
        This call always generates a rather large stacktrace, but succeeds nevertheless.
        Best find out why.
         */
        Editor textEditor = fileEditorManager.getSelectedTextEditor();
        if (textEditor == null){
            return null;
        }
        VisualPosition blockStart = correctFor0BasedVS1Based(textEditor.getSelectionModel().getSelectionStartPosition());
        VisualPosition blockEnd = correctFor0BasedVS1Based(textEditor.getSelectionModel().getSelectionEndPosition());
        if (blockStart == null || blockEnd == null){
            return null;
        }

        PsiFile psiFile = element.getContainingFile();
        final String canonicalPath = psiFile.getVirtualFile().getCanonicalPath();
        if (canonicalPath == null){
            return "canonical path is null";
        }

        final Project project = psiFile.getProject();
        final String workDir = ExecUtil.guessWorkDir(psiFile);
        if (ToolKey.GHC_MODI_KEY.getPath(project) != null) {
            final Module module = ModuleUtilCore.findModuleForPsiElement(psiFile);
            if (module != null) {
                GhcModi ghcModi = module.getComponent(GhcModi.class);
                if (ghcModi != null) {
                    return GhcModi.getFutureType(project, ghcModi.type(canonicalPath,
                            blockStart,blockEnd));

                } else {
                    return "ghcModi == null";
                }
            } else {
                return "module == null";
            }
        } else {
            return GhcMod.type(project, workDir, canonicalPath, blockStart,blockEnd);
        }
    }

    private static VisualPosition correctFor0BasedVS1Based(VisualPosition pos0Based) {
        if (pos0Based == null){
            return null;
        }
        return new VisualPosition(pos0Based.line+1,pos0Based.column);
    }

    @Override
    public List<String> getUrlFor(PsiElement element, PsiElement originalElement) {
        return Lists.newArrayList("getUrlFor result : koekoek");
    }

    @Override
    public String generateDoc(PsiElement element, @Nullable PsiElement originalElement) {
       return getTypeInfo(element);
    }



}
