// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellInstancedecl extends HaskellCompositeElement {

  @Nullable
  HaskellCtype getCtype();

  @Nullable
  HaskellIdecl getIdecl();

  @NotNull
  List<HaskellPpragma> getPpragmaList();

  @NotNull
  PsiElement getInstance();

  @Nullable
  PsiElement getWhere();

  @Nullable
  PsiElement getWhitespacelbracetok();

  @Nullable
  PsiElement getWhitespacerbracetok();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getRbrace();

}
