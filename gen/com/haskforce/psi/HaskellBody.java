// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellBody extends PsiElement {

  @NotNull
  List<HaskellCpp> getCppList();

  @NotNull
  List<HaskellImpdecl> getImpdeclList();

  @NotNull
  List<HaskellNcomment> getNcommentList();

  @NotNull
  List<HaskellPpragma> getPpragmaList();

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

  @NotNull
  List<HaskellSymbol> getSymbolList();

  @NotNull
  List<HaskellWhitechar> getWhitecharList();

}
