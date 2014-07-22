// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellClasss extends PsiElement {

  @NotNull
  List<HaskellAtype> getAtypeList();

  @Nullable
  HaskellClasss getClasss();

  @Nullable
  HaskellCtype getCtype();

  @Nullable
  HaskellQtycls getQtycls();

  @Nullable
  HaskellTyvar getTyvar();

  @Nullable
  PsiElement getDoublecolon();

  @Nullable
  PsiElement getLparen();

  @Nullable
  PsiElement getQuestion();

  @Nullable
  PsiElement getRparen();

}
