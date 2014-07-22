// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellAlts extends PsiElement {

  @Nullable
  HaskellAlts getAlts();

  @Nullable
  HaskellExp getExp();

  @Nullable
  HaskellFunorpatdecl getFunorpatdecl();

  @Nullable
  HaskellGendecl getGendecl();

  @NotNull
  List<HaskellGuard> getGuardList();

  @NotNull
  HaskellPat getPat();

  @Nullable
  HaskellPpragma getPpragma();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getPipe();

  @Nullable
  PsiElement getRbrace();

  @Nullable
  PsiElement getRightarrow();

}
