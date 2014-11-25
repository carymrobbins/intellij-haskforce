// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellClassdecl extends HaskellCompositeElement {

  @NotNull
  List<HaskellCdecl> getCdeclList();

  @Nullable
  HaskellCtype getCtype();

  @Nullable
  PsiElement getWhere();

  @Nullable
  PsiElement getWhitespacelbracetok();

  @Nullable
  PsiElement getWhitespacerbracetok();

  @NotNull
  PsiElement getClasstoken();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getRbrace();

}
