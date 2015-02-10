// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellBody extends HaskellCompositeElement {

  @NotNull
  List<HaskellAlt> getAltList();

  @NotNull
  List<HaskellClassdecl> getClassdeclList();

  @NotNull
  List<HaskellDatadecl> getDatadeclList();

  @NotNull
  List<HaskellDefaultdecl> getDefaultdeclList();

  @NotNull
  List<HaskellDerivingdecl> getDerivingdeclList();

  @NotNull
  List<HaskellExp> getExpList();

  @NotNull
  List<HaskellForeigndecl> getForeigndeclList();

  @NotNull
  List<HaskellFunorpatdecl> getFunorpatdeclList();

  @NotNull
  List<HaskellGendecl> getGendeclList();

  @NotNull
  List<HaskellImpdecl> getImpdeclList();

  @NotNull
  List<HaskellInstancedecl> getInstancedeclList();

  @NotNull
  List<HaskellLetexp> getLetexpList();

  @NotNull
  List<HaskellNewtypedecl> getNewtypedeclList();

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

  @NotNull
  List<HaskellTypedecl> getTypedeclList();

  @NotNull
  List<HaskellVarid> getVaridList();

  @NotNull
  List<HaskellVarsym> getVarsymList();

}
