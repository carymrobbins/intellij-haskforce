// This is a generated file. Not intended for manual editing.
package com.haskforce.haskell.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellGuard extends HaskellCompositeElement {

  @NotNull
  List<HaskellAlt> getAltList();

  @Nullable
  HaskellExp getExp();

  @Nullable
  HaskellFunorpatdecl getFunorpatdecl();

  @Nullable
  HaskellGendecl getGendecl();

  @Nullable
  HaskellLetexp getLetexp();

  @NotNull
  List<HaskellListlike> getListlikeList();

  @Nullable
  HaskellPat getPat();

  @NotNull
  List<HaskellPpragma> getPpragmaList();

  @NotNull
  List<HaskellPstringtoken> getPstringtokenList();

  @NotNull
  List<HaskellQcon> getQconList();

  @NotNull
  List<HaskellQop> getQopList();

  @NotNull
  List<HaskellQqblob> getQqblobList();

  @NotNull
  List<HaskellQvar> getQvarList();

  @NotNull
  List<HaskellQvarid> getQvaridList();

  @Nullable
  HaskellStmts getStmts();

  @NotNull
  List<HaskellVarid> getVaridList();

  @NotNull
  List<HaskellVarsym> getVarsymList();

  @Nullable
  PsiElement getCase();

  @Nullable
  PsiElement getDo();

  @Nullable
  PsiElement getElse();

  @Nullable
  PsiElement getIf();

  @Nullable
  PsiElement getLet();

  @Nullable
  PsiElement getMdotok();

  @Nullable
  PsiElement getOf();

  @Nullable
  PsiElement getThen();

  @Nullable
  PsiElement getWhitespacelbracetok();

  @Nullable
  PsiElement getWhitespacerbracetok();

  @Nullable
  PsiElement getBackslash();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getLeftarrow();

  @Nullable
  PsiElement getRbrace();

}
