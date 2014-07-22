// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellExport extends PsiElement {

  @Nullable
  HaskellCnames getCnames();

  @NotNull
  List<HaskellCpp> getCppList();

  @Nullable
  HaskellExport getExport();

  @Nullable
  HaskellQconid getQconid();

  @Nullable
  HaskellQtycls getQtycls();

  @Nullable
  HaskellQtycon getQtycon();

  @Nullable
  HaskellQvar getQvar();

  @Nullable
  HaskellQvars getQvars();

}
