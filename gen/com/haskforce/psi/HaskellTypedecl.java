// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellTypedecl extends HaskellCompositeElement {

  @Nullable
  HaskellCtype getCtype();

  @NotNull
  List<HaskellTvBndr> getTvBndrList();

  @NotNull
  List<HaskellTypee> getTypeeList();

  @Nullable
  PsiElement getFamilytoken();

  @Nullable
  PsiElement getForalltoken();

  @Nullable
  PsiElement getInstance();

  @NotNull
  PsiElement getType();

  @Nullable
  PsiElement getEquals();

  @Nullable
  PsiElement getPeriod();

}
