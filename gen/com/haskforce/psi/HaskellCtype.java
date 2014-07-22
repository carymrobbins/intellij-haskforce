// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellCtype extends PsiElement {

  @Nullable
  HaskellContext getContext();

  @Nullable
  HaskellCtype getCtype();

  @NotNull
  List<HaskellTvBndr> getTvBndrList();

  @Nullable
  HaskellTypee getTypee();

  @Nullable
  PsiElement getDoublearrow();

  @Nullable
  PsiElement getPeriod();

}
