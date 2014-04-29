// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellLexp extends PsiElement {

  @Nullable
  HaskellAlts getAlts();

  @NotNull
  List<HaskellApat> getApatList();

  @Nullable
  HaskellExp getExp();

  @Nullable
  HaskellFexp getFexp();

  @NotNull
  List<HaskellNcomment> getNcommentList();

  @NotNull
  List<HaskellPragma> getPragmaList();

  @NotNull
  List<HaskellPstringtoken> getPstringtokenList();

  @NotNull
  List<HaskellQconid> getQconidList();

  @NotNull
  List<HaskellQconsym> getQconsymList();

  @NotNull
  List<HaskellQinfixconid> getQinfixconidList();

  @NotNull
  List<HaskellQinfixvarid> getQinfixvaridList();

  @NotNull
  List<HaskellQvarid> getQvaridList();

  @NotNull
  List<HaskellQvarsym> getQvarsymList();

  @NotNull
  List<HaskellReservedop> getReservedopList();

  @NotNull
  List<HaskellSpecial> getSpecialList();

  @Nullable
  HaskellStmts getStmts();

  @NotNull
  List<HaskellWhitechar> getWhitecharList();

  @Nullable
  PsiElement getBackslash();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getRbrace();

  @Nullable
  PsiElement getRightarrow();

}
