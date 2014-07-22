// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellExp extends PsiElement {

  @Nullable
  HaskellAlts getAlts();

  @Nullable
  HaskellClassdecl getClassdecl();

  @Nullable
  HaskellContext getContext();

  @Nullable
  HaskellCtype getCtype();

  @Nullable
  HaskellDatadecl getDatadecl();

  @Nullable
  HaskellDefaultdecl getDefaultdecl();

  @Nullable
  HaskellDerivingdecl getDerivingdecl();

  @NotNull
  List<HaskellExp> getExpList();

  @Nullable
  HaskellForeigndecl getForeigndecl();

  @Nullable
  HaskellFunorpatdecl getFunorpatdecl();

  @Nullable
  HaskellGendecl getGendecl();

  @Nullable
  HaskellInstancedecl getInstancedecl();

  @Nullable
  HaskellNewtypedecl getNewtypedecl();

  @NotNull
  List<HaskellPat> getPatList();

  @Nullable
  HaskellPpragma getPpragma();

  @NotNull
  List<HaskellPstringtoken> getPstringtokenList();

  @NotNull
  List<HaskellQcon> getQconList();

  @NotNull
  List<HaskellQop> getQopList();

  @NotNull
  List<HaskellQvar> getQvarList();

  @Nullable
  HaskellStmts getStmts();

  @Nullable
  HaskellTypedecl getTypedecl();

  @Nullable
  HaskellTypee getTypee();

  @NotNull
  List<HaskellVarid> getVaridList();

  @NotNull
  List<HaskellVarsym> getVarsymList();

  @Nullable
  PsiElement getBackslash();

  @Nullable
  PsiElement getDollar();

  @Nullable
  PsiElement getDoublearrow();

  @Nullable
  PsiElement getDoublecolon();

  @Nullable
  PsiElement getDoubleperiod();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getLthopen();

  @Nullable
  PsiElement getLunboxparen();

  @Nullable
  PsiElement getRbrace();

  @Nullable
  PsiElement getRunboxparen();

  @Nullable
  PsiElement getSinglequote();

  @Nullable
  PsiElement getThquote();

}
