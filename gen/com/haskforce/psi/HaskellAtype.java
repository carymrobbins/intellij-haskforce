// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellAtype extends HaskellCompositeElement {

  @Nullable
  HaskellAtype getAtype();

  @NotNull
  List<HaskellCtype> getCtypeList();

  @Nullable
  HaskellKind getKind();

  @Nullable
  HaskellOqtycon getOqtycon();

  @Nullable
  HaskellPpragma getPpragma();

  @Nullable
  HaskellPstringtoken getPstringtoken();

  @NotNull
  List<HaskellTvBndr> getTvBndrList();

  @NotNull
  List<HaskellTypee> getTypeeList();

  @Nullable
  HaskellTyvar getTyvar();

  @NotNull
  List<HaskellVars> getVarsList();

  @Nullable
  PsiElement getForalltoken();

  @Nullable
  PsiElement getDoublehash();

  @Nullable
  PsiElement getExclamation();

  @Nullable
  PsiElement getHash();

  @Nullable
  PsiElement getIntegertoken();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getLbracket();

  @Nullable
  PsiElement getLparen();

  @Nullable
  PsiElement getLunboxparen();

  @Nullable
  PsiElement getPeriod();

  @Nullable
  PsiElement getQuestion();

  @Nullable
  PsiElement getRbrace();

  @Nullable
  PsiElement getRbracket();

  @Nullable
  PsiElement getRightarrow();

  @Nullable
  PsiElement getRparen();

  @Nullable
  PsiElement getRunboxparen();

  @Nullable
  PsiElement getSinglequote();

}
