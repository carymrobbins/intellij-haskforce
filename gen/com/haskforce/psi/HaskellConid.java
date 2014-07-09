// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiReference;

public interface HaskellConid extends PsiNamedElement {

  @NotNull
  PsiElement getConidRegexp();

  @NotNull
  String getName();

  @NotNull
  PsiReference getReference();

  @NotNull
  PsiElement setName(String newName);

}
