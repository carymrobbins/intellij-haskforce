// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.navigation.ItemPresentation;
import com.intellij.psi.PsiReference;

public interface HaskellVarid extends PsiNamedElement {

  @NotNull
  PsiElement getVaridRegexp();

  @NotNull
  String getName();

  @NotNull
  PsiReference getReference();

  @NotNull
  PsiElement setName(String newName);

  @NotNull
  ItemPresentation getPresentation();

}
