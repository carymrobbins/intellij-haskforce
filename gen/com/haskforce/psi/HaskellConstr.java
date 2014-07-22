// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellConstr extends PsiElement {

  @NotNull
  List<HaskellAtype> getAtypeList();

  @Nullable
  HaskellCon getCon();

  @Nullable
  HaskellConop getConop();

  @NotNull
  List<HaskellPpragma> getPpragmaList();

  @NotNull
  List<HaskellTypee> getTypeeList();

  @NotNull
  List<HaskellVars> getVarsList();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getRbrace();

}
