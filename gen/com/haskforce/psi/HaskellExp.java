// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellExp extends HaskellCompositeElement {

  @NotNull
  List<HaskellAlt> getAltList();

  @Nullable
  HaskellClscontext getClscontext();

  @NotNull
  List<HaskellCtype> getCtypeList();

  @Nullable
  HaskellExp getExp();

  @NotNull
  List<HaskellLabel> getLabelList();

  @Nullable
  HaskellLetexp getLetexp();

  @NotNull
  List<HaskellListlike> getListlikeList();

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
  HaskellTypee getTypee();

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
  PsiElement getLcasetok();

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
  PsiElement getDoublearrow();

  @Nullable
  PsiElement getDoublecolon();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getRbrace();

}
