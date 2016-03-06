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
  List<HaskellExp> getExpList();

  @NotNull
  List<HaskellLetexp> getLetexpList();

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

  @NotNull
  List<HaskellStmts> getStmtsList();

  @Nullable
  HaskellTypee getTypee();

  @NotNull
  List<HaskellVarid> getVaridList();

  @NotNull
  List<HaskellVarsym> getVarsymList();

  @Nullable
  PsiElement getDoublearrow();

  @Nullable
  PsiElement getDoublecolon();

}
