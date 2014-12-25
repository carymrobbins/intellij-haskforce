package com.haskforce.highlighting.annotation;

import com.google.common.collect.Lists;
import com.haskforce.highlighting.annotation.external.GhcMod;
import com.haskforce.highlighting.annotation.external.GhcModi;
import com.haskforce.highlighting.annotation.external.HaskellExternalAnnotator;
import com.haskforce.highlighting.annotation.external.TypeInfoUtil;
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
        Project project = element.getProject();
        return TypeInfoUtil.getTypeInfo(project);
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
