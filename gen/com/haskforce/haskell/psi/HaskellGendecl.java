// This is a generated file. Not intended for manual editing.
package com.haskforce.haskell.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellGendecl extends HaskellCompositeElement {

  @Nullable
  HaskellCtype getCtype();

  @Nullable
  HaskellFixity getFixity();

  @NotNull
  List<HaskellOp> getOpList();

  @Nullable
  HaskellVars getVars();

  @Nullable
  PsiElement getDoublecolon();

  @Nullable
  PsiElement getIntegertoken();

}
