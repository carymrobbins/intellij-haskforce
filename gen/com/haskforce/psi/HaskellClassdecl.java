// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellClassdecl extends HaskellCompositeElement {

  @Nullable
  HaskellCdecl getCdecl();

  @Nullable
  HaskellCtype getCtype();

  @NotNull
  List<HaskellTyvar> getTyvarList();

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
  PsiElement getPipe();

  @Nullable
  PsiElement getRbrace();

}
