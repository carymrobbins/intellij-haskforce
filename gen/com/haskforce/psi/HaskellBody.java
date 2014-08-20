// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellBody extends PsiElement {

  @NotNull
  List<HaskellAlt> getAltList();

  @Nullable
  HaskellClassdecl getClassdecl();

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

  @NotNull
  List<HaskellImpdecl> getImpdeclList();

  @Nullable
  HaskellInstancedecl getInstancedecl();

  @Nullable
  HaskellNewtypedecl getNewtypedecl();

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
  HaskellTypedecl getTypedecl();

  @NotNull
  List<HaskellVarid> getVaridList();

  @NotNull
  List<HaskellVarsym> getVarsymList();

  @Nullable
  PsiElement getBackslash();

  @Nullable
  PsiElement getDoubleperiod();

  @Nullable
  PsiElement getIdsplice();

  @Nullable
  PsiElement getLunboxparen();

  @Nullable
  PsiElement getParensplice();

  @Nullable
  PsiElement getRunboxparen();

  @Nullable
  PsiElement getSinglequote();

  @Nullable
  PsiElement getThquote();

}
