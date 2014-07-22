// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellFtype extends PsiElement {

  @NotNull
  List<HaskellAtype> getAtypeList();

  @Nullable
  HaskellFtype getFtype();

  @Nullable
  HaskellQtycon getQtycon();

  @Nullable
  PsiElement getRightarrow();

}
