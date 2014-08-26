// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellExp extends PsiElement {

  @NotNull
  List<HaskellAlt> getAltList();

  @Nullable
  HaskellContext getContext();

  @NotNull
  List<HaskellExp> getExpList();

  @Nullable
  HaskellFunorpatdecl getFunorpatdecl();

  @Nullable
  HaskellGendecl getGendecl();

  @Nullable
  HaskellImpdecl getImpdecl();

  @NotNull
  List<HaskellPat> getPatList();

  @NotNull
  List<HaskellPpragma> getPpragmaList();

  @NotNull
  List<HaskellPstringtoken> getPstringtokenList();

  @NotNull
  List<HaskellQcon> getQconList();

  @NotNull
  List<HaskellQop> getQopList();

  @NotNull
  List<HaskellQvar> getQvarList();

  @NotNull
  List<HaskellQvarid> getQvaridList();

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
  PsiElement getIdsplice();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getLunboxparen();

  @Nullable
  PsiElement getParensplice();

  @Nullable
  PsiElement getRbrace();

  @Nullable
  PsiElement getRunboxparen();

  @Nullable
  PsiElement getSinglequote();

  @Nullable
  PsiElement getThquote();

}
