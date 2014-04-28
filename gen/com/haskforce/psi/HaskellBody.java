// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellBody extends PsiElement {

  @NotNull
  List<HaskellCdecl> getCdeclList();

  @NotNull
  List<HaskellConstrs> getConstrsList();

  @NotNull
  List<HaskellContext> getContextList();

  @NotNull
  List<HaskellFdecl> getFdeclList();

  @NotNull
  List<HaskellIdecl> getIdeclList();

  @NotNull
  List<HaskellImpdecl> getImpdeclList();

  @NotNull
  List<HaskellInst> getInstList();

  @NotNull
  List<HaskellNcomment> getNcommentList();

  @NotNull
  List<HaskellNewconstr> getNewconstrList();

  @NotNull
  List<HaskellPragma> getPragmaList();

  @NotNull
  List<HaskellQconid> getQconidList();

  @NotNull
  List<HaskellQconsym> getQconsymList();

  @NotNull
  List<HaskellQinfixconid> getQinfixconidList();

  @NotNull
  List<HaskellQinfixvarid> getQinfixvaridList();

  @NotNull
  List<HaskellQtycls> getQtyclsList();

  @NotNull
  List<HaskellQvarid> getQvaridList();

  @NotNull
  List<HaskellQvarsym> getQvarsymList();

  @NotNull
  List<HaskellReservedop> getReservedopList();

  @NotNull
  List<HaskellScontext> getScontextList();

  @NotNull
  List<HaskellSpecial> getSpecialList();

  @NotNull
  List<HaskellTycls> getTyclsList();

  @NotNull
  List<HaskellTycon> getTyconList();

  @NotNull
  List<HaskellTypee> getTypeeList();

  @NotNull
  List<HaskellTyvar> getTyvarList();

  @NotNull
  List<HaskellWhitechar> getWhitecharList();

}
