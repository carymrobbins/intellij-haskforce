// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellNewtypedecl extends PsiElement {

  @Nullable
  HaskellContext getContext();

  @Nullable
  HaskellNewconstr getNewconstr();

  @Nullable
  HaskellQtycls getQtycls();

  @Nullable
  HaskellTycon getTycon();

  @NotNull
  List<HaskellTyvar> getTyvarList();

  @Nullable
  PsiElement getDoublearrow();

  @Nullable
  PsiElement getEquals();

  @Nullable
  PsiElement getLparen();

  @Nullable
  PsiElement getRparen();

}
