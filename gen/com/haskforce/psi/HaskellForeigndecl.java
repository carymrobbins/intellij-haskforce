// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellForeigndecl extends HaskellCompositeElement {

  @NotNull
  List<HaskellAtype> getAtypeList();

  @Nullable
  HaskellClscontext getClscontext();

  @Nullable
  HaskellLabel getLabel();

  @Nullable
  HaskellPstringtoken getPstringtoken();

  @Nullable
  HaskellQtycon getQtycon();

  @Nullable
  HaskellTyvar getTyvar();

  @Nullable
  HaskellVarid getVarid();

  @Nullable
  HaskellVarsym getVarsym();

  @Nullable
  PsiElement getExporttoken();

  @NotNull
  PsiElement getForeign();

  @Nullable
  PsiElement getImport();

  @Nullable
  PsiElement getType();

  @Nullable
  PsiElement getDoublearrow();

  @Nullable
  PsiElement getDoublecolon();

  @Nullable
  PsiElement getLparen();

  @Nullable
  PsiElement getRparen();

}
