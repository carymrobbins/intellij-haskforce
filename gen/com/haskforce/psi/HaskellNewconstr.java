// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellNewconstr extends PsiElement {

  @Nullable
  HaskellAtype getAtype();

  @NotNull
  HaskellCon getCon();

  @Nullable
  HaskellTypee getTypee();

  @Nullable
  HaskellVar getVar();

  @Nullable
  PsiElement getDoublecolon();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getRbrace();

}
