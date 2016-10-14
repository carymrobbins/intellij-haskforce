package com.haskforce.haskell.highlighting.annotation;

import com.haskforce.system.integrations.typeInfo.TypeInfo;
import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.VisualPosition;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.Nullable;

/**
 * This is the first version of the DocumentationProvider, the tooltip that can be shown on
 * hovering (and is also bound to an action). Right now, it just tries to get the type info.
 */
public class HaskellDocumentationProvider extends AbstractDocumentationProvider {
    @Override
    public String generateDoc(PsiElement element, @Nullable PsiElement originalElement) {
        int startOffset = element.getTextRange().getStartOffset();
        int endOffset = element.getTextRange().getEndOffset();
        Module module = ModuleUtilCore.findModuleForPsiElement(element);
        FileDocumentManager fileDocumentManager= FileDocumentManager.getInstance();
        VirtualFile projectFile = element.getContainingFile().getVirtualFile();
        Document cachedDocument = fileDocumentManager.getCachedDocument(projectFile);
        if (cachedDocument == null) {
            return null;
        }
        int startLineNumber = cachedDocument.getLineNumber(startOffset);
        int endLineNumber = cachedDocument.getLineNumber(endOffset);
        int startColumn = startOffset - cachedDocument.getLineStartOffset(startLineNumber);
        int endColumn = endOffset - cachedDocument.getLineStartOffset(endLineNumber);

        // and also correct them for (0,0) vs (1,1) leftmost coordinate (intellij -> ghc)
        return TypeInfo.getTypeInfo(module,new VisualPosition(startLineNumber, startColumn),new VisualPosition(endLineNumber, endColumn), projectFile);
    }

}

