// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellLetexp extends HaskellCompositeElement {

  @Nullable
  HaskellExp getExp();

  @NotNull
  List<HaskellFunorpatdecl> getFunorpatdeclList();

  @NotNull
  List<HaskellGendecl> getGendeclList();

  @NotNull
  List<HaskellPpragma> getPpragmaList();

  @Nullable
  PsiElement getIn();

  @NotNull
  PsiElement getLet();

  @Nullable
  PsiElement getWhitespacelbracetok();

  @Nullable
  PsiElement getWhitespacerbracetok();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getRbrace();

}
