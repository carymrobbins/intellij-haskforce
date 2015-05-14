// This is a generated file. Not intended for manual editing.
package com.haskforce.cabal.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;

public interface CabalModule extends CabalNamedElement {

  @NotNull
  List<CabalVarid> getVaridList();

  @NotNull
  String getName();

  @NotNull
  PsiReference getReference();

  @Nullable
  PsiElement getNameIdentifier();

  @Nullable
  PsiElement setName(String newName);

}
