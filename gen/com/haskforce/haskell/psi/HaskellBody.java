// This is a generated file. Not intended for manual editing.
package com.haskforce.haskell.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellBody extends HaskellCompositeElement {

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

  @Nullable
  HaskellExp getExp();

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
  HaskellLetexp getLetexp();

  @NotNull
  List<HaskellListlike> getListlikeList();

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
  List<HaskellQqblob> getQqblobList();

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
  PsiElement getCase();

  @Nullable
  PsiElement getDo();

  @Nullable
  PsiElement getElse();

  @Nullable
  PsiElement getIf();

  @Nullable
  PsiElement getMdotok();

  @Nullable
  PsiElement getOf();

  @Nullable
  PsiElement getThen();

  @Nullable
  PsiElement getBackslash();

}
