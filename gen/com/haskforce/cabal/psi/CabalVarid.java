// This is a generated file. Not intended for manual editing.
package com.haskforce.cabal.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;

public interface CabalVarid extends CabalNamedElement {

  @NotNull
  PsiElement getVaridRegexp();

  @NotNull
  String getName();

  @NotNull
  PsiReference getReference();

  @Nullable
  PsiElement getNameIdentifier();

  @Nullable
  PsiElement setName(String newName);

}
