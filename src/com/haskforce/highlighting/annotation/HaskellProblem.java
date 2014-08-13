package com.haskforce.highlighting.annotation;

import com.intellij.lang.annotation.Annotation;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

public abstract class HaskellProblem {
    public int startLine;
    public int startColumn;

    @Nullable
    public abstract Annotation createAnnotation(@NotNull PsiFile file, @NotNull HaskellAnnotationHolder holder);

    public int getOffsetStart(final String fileText) {
        return StringUtil.lineColToOffset(fileText, startLine - 1, startColumn - 1);
    }
}
