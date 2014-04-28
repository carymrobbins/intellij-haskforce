// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellFunlhs extends PsiElement {

  @NotNull
  List<HaskellApat> getApatList();

  @Nullable
  HaskellFunlhs getFunlhs();

  @NotNull
  List<HaskellPat> getPatList();

  @Nullable
  HaskellVarop getVarop();

  @Nullable
  HaskellVarsym getVarsym();

  @Nullable
  PsiElement getLparen();

  @Nullable
  PsiElement getRparen();

  @Nullable
  PsiElement getVaridRegexp();

}
