// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellApat extends PsiElement {

  @Nullable
  HaskellApat getApat();

  @Nullable
  HaskellGcon getGcon();

  @NotNull
  List<HaskellPat> getPatList();

  @Nullable
  HaskellPstringtoken getPstringtoken();

  @Nullable
  HaskellQcon getQcon();

  @NotNull
  List<HaskellQvar> getQvarList();

  @Nullable
  HaskellVarsym getVarsym();

  @Nullable
  PsiElement getAmpersat();

  @Nullable
  PsiElement getChartoken();

  @Nullable
  PsiElement getFloattoken();

  @Nullable
  PsiElement getIntegertoken();

  @Nullable
  PsiElement getLbrace();

  @Nullable
  PsiElement getLbracket();

  @Nullable
  PsiElement getLparen();

  @Nullable
  PsiElement getRbrace();

  @Nullable
  PsiElement getRbracket();

  @Nullable
  PsiElement getRparen();

  @Nullable
  PsiElement getTilde();

  @Nullable
  PsiElement getVaridRegexp();

}
