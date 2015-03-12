// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellImportt extends HaskellCompositeElement {

  @NotNull
  List<HaskellCon> getConList();

  @Nullable
  HaskellTycon getTycon();

  @NotNull
  List<HaskellVarid> getVaridList();

  @Nullable
  HaskellVars getVars();

  @NotNull
  List<HaskellVarsym> getVarsymList();

  @Nullable
  PsiElement getDoubleperiod();

}
