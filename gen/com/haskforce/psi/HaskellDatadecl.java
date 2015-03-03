// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellDatadecl extends HaskellCompositeElement {

  @NotNull
  List<HaskellAtype> getAtypeList();

  @NotNull
  List<HaskellCon> getConList();

  @NotNull
  List<HaskellConstr> getConstrList();

  @NotNull
  List<HaskellContext> getContextList();

  @Nullable
  HaskellCtype getCtype();

  @Nullable
  HaskellKind getKind();

  @Nullable
  HaskellOqtycon getOqtycon();

  @NotNull
  List<HaskellQtycls> getQtyclsList();

  @NotNull
  List<HaskellTvBndr> getTvBndrList();

  @NotNull
  List<HaskellTypee> getTypeeList();

  @NotNull
  List<HaskellTyvar> getTyvarList();

  @NotNull
  List<HaskellVars> getVarsList();

  @NotNull
  PsiElement getData();

  @Nullable
  PsiElement getDeriving();

  @Nullable
  PsiElement getForalltoken();

  @Nullable
  PsiElement getInstance();

  @Nullable
  PsiElement getWhere();

  @Nullable
  PsiElement getWhitespacelbracetok();

  @Nullable
  PsiElement getWhitespacerbracetok();

  @Nullable
  PsiElement getWhitespacesemitok();

  @Nullable
  PsiElement getEquals();

  @Nullable
  PsiElement getLparen();

  @Nullable
  PsiElement getPeriod();

  @Nullable
  PsiElement getRparen();

  @Nullable
  PsiElement getSemicolon();

}
