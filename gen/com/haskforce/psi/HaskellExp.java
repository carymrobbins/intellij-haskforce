// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellExp extends PsiElement {

  @Nullable
  HaskellContext getContext();

  @NotNull
  HaskellInfixexp getInfixexp();

  @Nullable
  HaskellTypee getTypee();

  @Nullable
  PsiElement getDoublearrow();

  @Nullable
  PsiElement getDoublecolon();

}
