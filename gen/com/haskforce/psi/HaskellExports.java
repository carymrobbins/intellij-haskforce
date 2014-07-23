// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellExports extends PsiElement {

  @NotNull
  List<HaskellCpp> getCppList();

  @NotNull
  List<HaskellExport> getExportList();

  @NotNull
  PsiElement getLparen();

  @Nullable
  PsiElement getRparen();

}
