// This is a generated file. Not intended for manual editing.
package com.haskforce.haskell.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellCtype extends HaskellCompositeElement {

  @Nullable
  HaskellClscontext getClscontext();

  @Nullable
  HaskellCtype getCtype();

  @NotNull
  List<HaskellTvBndr> getTvBndrList();

  @Nullable
  HaskellTypee getTypee();

  @Nullable
  PsiElement getForalltoken();

  @Nullable
  PsiElement getDoublearrow();

  @Nullable
  PsiElement getPeriod();

}
