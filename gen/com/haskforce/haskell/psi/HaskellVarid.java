// This is a generated file. Not intended for manual editing.
package com.haskforce.haskell.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.StubBasedPsiElement;
import com.haskforce.haskell.stubs.HaskellVaridStub;
import com.intellij.navigation.ItemPresentation;
import com.intellij.psi.PsiReference;

public interface HaskellVarid extends HaskellNamedElement, StubBasedPsiElement<HaskellVaridStub> {

  @Nullable
  PsiElement getAs();

  @Nullable
  PsiElement getQualified();

  @Nullable
  PsiElement getRectok();

  @Nullable
  PsiElement getVaridRegexp();

  @NotNull
  String getName();

  @Nullable
  PsiElement getNameIdentifier();

  @NotNull
  PsiReference getReference();

  @Nullable
  PsiElement setName(String newName);

  @NotNull
  ItemPresentation getPresentation();

}
