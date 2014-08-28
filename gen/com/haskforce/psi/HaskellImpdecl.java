// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellImpdecl extends PsiElement {

  @NotNull
  List<HaskellImportt> getImporttList();

  @NotNull
  List<HaskellQconid> getQconidList();

  @Nullable
  PsiElement getLparen();

  @Nullable
  PsiElement getRparen();

}
