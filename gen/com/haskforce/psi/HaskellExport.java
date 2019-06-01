// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellExport extends HaskellCompositeElement {

  @NotNull
  List<HaskellCon> getConList();

  @NotNull
  List<HaskellLabel> getLabelList();

  @Nullable
  HaskellQconid getQconid();

  @Nullable
  HaskellQtycon getQtycon();

  @Nullable
  HaskellQvar getQvar();

  @Nullable
  HaskellQvars getQvars();

  @NotNull
  List<HaskellVarid> getVaridList();

  @NotNull
  List<HaskellVarsym> getVarsymList();

  @Nullable
  PsiElement getModuletoken();

  @Nullable
  PsiElement getDoubleperiod();

}
