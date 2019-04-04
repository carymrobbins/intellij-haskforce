package com.haskforce.highlighting.annotation;

import com.haskforce.actions.TypeInfoAction;
import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.Nullable;

/**
 * This is the first version of the DocumentationProvider, the tooltip that can be shown on
 * hovering (and is also bound to an action). Right now, it just tries to get the type info.
 */

public class HaskellDocumentationProvider extends AbstractDocumentationProvider {
    @Nullable
    @Override
    public String generateDoc(PsiElement element, @Nullable PsiElement originalElement) {
        FileDocumentManager fileDocumentManager= FileDocumentManager.getInstance();
        VirtualFile projectFile = element.getContainingFile().getVirtualFile();
        Document cachedDocument = fileDocumentManager.getCachedDocument(projectFile);
        if (cachedDocument == null) return null;
        return TypeInfoAction.getTypeInfo(element.getProject());
    }
}
