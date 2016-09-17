// This is a generated file. Not intended for manual editing.
package com.haskforce.haskell.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellNewtypedecl extends HaskellCompositeElement {

  @Nullable
  HaskellClscontext getClscontext();

  @Nullable
  HaskellNewconstr getNewconstr();

  @NotNull
  List<HaskellQtycls> getQtyclsList();

  @Nullable
  HaskellTycon getTycon();

  @NotNull
  List<HaskellTyvar> getTyvarList();

  @Nullable
  PsiElement getDeriving();

  @NotNull
  PsiElement getNewtype();

  @Nullable
  PsiElement getDoublearrow();

  @Nullable
  PsiElement getEquals();

  @Nullable
  PsiElement getLparen();

  @Nullable
  PsiElement getRparen();

}
