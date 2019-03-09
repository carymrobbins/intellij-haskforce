package com.haskforce.haskell.lang.parser;

import com.haskforce.HaskellFileType;
import com.haskforce.HaskellLanguage;
import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import org.jetbrains.annotations.NotNull;

public class HaskellFile2 extends PsiFileBase {
  protected HaskellFile2(@NotNull FileViewProvider viewProvider) {
    super(viewProvider, HaskellLanguage.INSTANCE);
  }

  @NotNull
  @Override
  public FileType getFileType() {
    return HaskellFileType.INSTANCE;
  }
}
