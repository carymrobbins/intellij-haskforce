package com.haskforce.highlighting.annotation;

import com.haskforce.highlighting.annotation.external.TypeInfoUtil;
import com.intellij.lang.documentation.AbstractDocumentationProvider;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.editor.VisualPosition;
import com.intellij.openapi.fileEditor.FileDocumentManager;
import com.intellij.openapi.fileEditor.FileEditorManager;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.VirtualFile;
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
        int startOffset = element.getTextRange().getStartOffset();
        int endOffset = element.getTextRange().getEndOffset();
        Project project = element.getProject();
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
        VisualPosition startPosition = TypeInfoUtil.correctFor0BasedVS1Based(new VisualPosition(startLineNumber, startColumn));
        VisualPosition endPosition = TypeInfoUtil.correctFor0BasedVS1Based(new VisualPosition(endLineNumber, endColumn));
        return TypeInfoUtil.getTypeInfo(project,startPosition,endPosition, projectFile);
    }

}

