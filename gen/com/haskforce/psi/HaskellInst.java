// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellInst extends PsiElement {

  @Nullable
  HaskellQtycon getQtycon();

  @NotNull
  List<HaskellTyvar> getTyvarList();

  @Nullable
  PsiElement getLbracket();

  @Nullable
  PsiElement getLparen();

  @Nullable
  PsiElement getRbracket();

  @Nullable
  PsiElement getRightarrow();

  @Nullable
  PsiElement getRparen();

}
