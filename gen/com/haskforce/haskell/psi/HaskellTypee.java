// This is a generated file. Not intended for manual editing.
package com.haskforce.haskell.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellTypee extends HaskellCompositeElement {

  @NotNull
  List<HaskellAtype> getAtypeList();

  @Nullable
  HaskellQconop getQconop();

  @Nullable
  HaskellQtyconop getQtyconop();

  @Nullable
  HaskellTypee getTypee();

  @Nullable
  HaskellVarop getVarop();

  @Nullable
  PsiElement getRightarrow();

  @Nullable
  PsiElement getSinglequote();

}
