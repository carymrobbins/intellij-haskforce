// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.StubBasedPsiElement;
import com.haskforce.stubs.HaskellQconidStub;
import com.intellij.navigation.ItemPresentation;
import com.intellij.psi.PsiReference;

public interface HaskellQconid extends HaskellNamedElement, StubBasedPsiElement<HaskellQconidStub> {

  @NotNull
  List<HaskellConid> getConidList();

  @Nullable
  PsiElement getHash();

  @Nullable
  PsiElement getNameIdentifier();

  @NotNull
  String getName();

  @Nullable
  PsiElement setName(String newName);

  @NotNull
  PsiReference getReference();

  @NotNull
  ItemPresentation getPresentation();

}
