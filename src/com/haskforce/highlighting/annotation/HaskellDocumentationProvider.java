package com.haskforce.highlighting.annotation;

import com.haskforce.highlighting.annotation.external.TypeInfoUtil;
import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiManager;
import org.jetbrains.annotations.Nullable;

/**
 * This is the first version of the DocumentationProvider, the tooltip that can be shown on
 * hovering (and is also bound to an action). Right now, it just tries to get the type info.
 */

public class HaskellDocumentationProvider extends AbstractDocumentationProvider {
    @Override
    public String generateDoc(PsiElement element, @Nullable PsiElement originalElement) {
        Project project = element.getProject();
        return TypeInfoUtil.getTypeInfo(project);
    }

}

