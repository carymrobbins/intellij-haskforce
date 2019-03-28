// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellDerivingdecl extends HaskellCompositeElement {

  @NotNull
  List<HaskellCtype> getCtypeList();

  @Nullable
  HaskellPpragma getPpragma();

  @Nullable
  HaskellVarid getVarid();

  @NotNull
  PsiElement getDeriving();

  @Nullable
  PsiElement getInstance();

  @Nullable
  PsiElement getNewtype();

}
