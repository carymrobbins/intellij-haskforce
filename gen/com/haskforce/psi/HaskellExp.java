// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellExp extends PsiElement {

  @Nullable
  HaskellAlts getAlts();

  @Nullable
  HaskellContext getContext();

  @Nullable
  HaskellExp getExp();

  @Nullable
  HaskellFunorpatdecl getFunorpatdecl();

  @Nullable
  HaskellGendecl getGendecl();

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
  HaskellTypee getTypee();

  @NotNull
  List<HaskellVarid> getVaridList();

  @NotNull
  List<HaskellVarsym> getVarsymList();

  @Nullable
  PsiElement getBackslash();

  @Nullable
  PsiElement getDoublearrow();

  @Nullable
  PsiElement getDoublecolon();

  @Nullable
  PsiElement getDoubleperiod();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getLunboxparen();

  @Nullable
  PsiElement getRbrace();

  @Nullable
  PsiElement getRunboxparen();

  @Nullable
  PsiElement getSemicolon();

  @Nullable
  PsiElement getSinglequote();

  @Nullable
  PsiElement getThquote();

}
