package com.haskforce.system.integrations.highlighting;

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

/**
 * An represents an Annotation in an Haskell-File
 */
public abstract class HaskellProblem {
    /**
     * the startLine, starting at 1
     */
    public int startLine;
    /**
     * the startColumn, starting at 1
     */
    public int startColumn;

    /**
     * creates the actual Annotation
     * @param file the PsiFile to create the annotation for
     * @param holder the AnnotationHolder
     */
    public abstract void createAnnotations(@NotNull PsiFile file, @NotNull HaskellAnnotationHolder holder);

    /**
     * returns the offset for the Annotation
     */
    public int getOffsetStart(final String fileText) {
        return StringUtil.lineColToOffset(fileText, startLine - 1, startColumn - 1);
    }
}
