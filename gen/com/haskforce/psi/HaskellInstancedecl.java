// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellInstancedecl extends PsiElement {

  @Nullable
  HaskellCtype getCtype();

  @Nullable
  HaskellIdecl getIdecl();

  @NotNull
  List<HaskellPpragma> getPpragmaList();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getRbrace();

  @Nullable
  PsiElement getSemicolon();

}
