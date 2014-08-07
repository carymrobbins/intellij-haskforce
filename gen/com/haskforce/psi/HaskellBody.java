// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellBody extends PsiElement {

  @Nullable
  HaskellClassdecl getClassdecl();

  @Nullable
  HaskellDatadecl getDatadecl();

  @Nullable
  HaskellDefaultdecl getDefaultdecl();

  @Nullable
  HaskellDerivingdecl getDerivingdecl();

  @Nullable
  HaskellForeigndecl getForeigndecl();

  @Nullable
  HaskellFunorpatdecl getFunorpatdecl();

  @Nullable
  HaskellGendecl getGendecl();

  @Nullable
  HaskellImpdecl getImpdecl();

  @Nullable
  HaskellInstancedecl getInstancedecl();

  @Nullable
  HaskellNewtypedecl getNewtypedecl();

  @NotNull
  List<HaskellPpragma> getPpragmaList();

  @Nullable
  HaskellTypedecl getTypedecl();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getRbrace();

}
