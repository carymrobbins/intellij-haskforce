// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellNewtypedecl extends HaskellCompositeElement {

  @Nullable
  HaskellClscontext getClscontext();

  @NotNull
  List<HaskellCtype> getCtypeList();

  @Nullable
  HaskellNewconstr getNewconstr();

  @NotNull
  List<HaskellQtycls> getQtyclsList();

  @Nullable
  HaskellTycon getTycon();

  @NotNull
  List<HaskellTyvar> getTyvarList();

  @NotNull
  List<HaskellVarid> getVaridList();

  @Nullable
  PsiElement getDoublearrow();

  @Nullable
  PsiElement getEquals();

}
