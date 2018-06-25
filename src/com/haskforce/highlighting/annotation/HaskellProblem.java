package com.haskforce.highlighting.annotation;

import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;
import org.jetbrains.annotations.NotNull;

public interface HaskellProblem {

  int getStartLine();

  int getStartColumn();

  void createAnnotations(@NotNull PsiFile file, @NotNull HaskellAnnotationHolder holder);

  default int getOffsetStart(String fileText) {
    return StringUtil.lineColToOffset(fileText, getStartLine() - 1, getStartColumn() - 1);
  }
}
