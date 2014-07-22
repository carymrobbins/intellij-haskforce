// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellLpat extends PsiElement {

  @NotNull
  List<HaskellLpat> getLpatList();

  @Nullable
  HaskellPstringtoken getPstringtoken();

  @Nullable
  HaskellQcon getQcon();

  @NotNull
  List<HaskellQconop> getQconopList();

  @NotNull
  List<HaskellQvar> getQvarList();

  @Nullable
  HaskellVarid getVarid();

  @Nullable
  HaskellVarsym getVarsym();

  @Nullable
  PsiElement getAmpersat();

  @Nullable
  PsiElement getChartoken();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getLbracket();

  @Nullable
  PsiElement getLparen();

  @Nullable
  PsiElement getMinus();

  @Nullable
  PsiElement getRbrace();

  @Nullable
  PsiElement getRbracket();

  @Nullable
  PsiElement getRparen();

  @Nullable
  PsiElement getTilde();

  @Nullable
  PsiElement getUnderscore();

}
