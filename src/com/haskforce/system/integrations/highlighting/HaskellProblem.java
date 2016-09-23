package com.haskforce.system.integrations.highlighting;

import com.haskforce.haskell.highlighting.annotation.HaskellAnnotationHolder;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

public abstract class HaskellProblem {
    public int startLine;
    public int startColumn;

    public abstract void createAnnotations(@NotNull PsiFile file, @NotNull HaskellAnnotationHolder holder);

    public int getOffsetStart(final String fileText) {
        return StringUtil.lineColToOffset(fileText, startLine - 1, startColumn - 1);
    }
}
