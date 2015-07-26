// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellTypedecl extends HaskellCompositeElement {

  @Nullable
  HaskellCtype getCtype();

  @Nullable
  HaskellFunorpatdecl getFunorpatdecl();

  @Nullable
  HaskellGendecl getGendecl();

  @NotNull
  List<HaskellKind> getKindList();

  @NotNull
  List<HaskellPpragma> getPpragmaList();

  @NotNull
  List<HaskellTvBndr> getTvBndrList();

  @NotNull
  List<HaskellTypee> getTypeeList();

  @NotNull
  List<HaskellTyvar> getTyvarList();

  @Nullable
  PsiElement getFamilytoken();

  @Nullable
  PsiElement getForalltoken();

  @Nullable
  PsiElement getInstance();

  @NotNull
  PsiElement getType();

  @Nullable
  PsiElement getWhere();

  @Nullable
  PsiElement getWhitespacelbracetok();

  @Nullable
  PsiElement getWhitespacerbracetok();

  @Nullable
  PsiElement getEquals();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getPeriod();

  @Nullable
  PsiElement getRbrace();

}
