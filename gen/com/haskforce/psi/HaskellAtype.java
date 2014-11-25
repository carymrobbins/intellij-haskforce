// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellAtype extends HaskellCompositeElement {

  @NotNull
  List<HaskellAtype> getAtypeList();

  @NotNull
  List<HaskellCtype> getCtypeList();

  @Nullable
  HaskellKind getKind();

  @Nullable
  HaskellOqtycon getOqtycon();

  @Nullable
  HaskellPstringtoken getPstringtoken();

  @NotNull
  List<HaskellTypee> getTypeeList();

  @Nullable
  HaskellTyvar getTyvar();

  @NotNull
  List<HaskellVars> getVarsList();

  @Nullable
  PsiElement getDoublehash();

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
