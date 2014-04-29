// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellAexp extends PsiElement {

  @NotNull
  List<HaskellAexp> getAexpList();

  @Nullable
  HaskellExp getExp();

  @Nullable
  HaskellGcon getGcon();

  @Nullable
  HaskellInfixexp getInfixexp();

  @Nullable
  HaskellPstringtoken getPstringtoken();

  @Nullable
  HaskellQcon getQcon();

  @NotNull
  List<HaskellQop> getQopList();

  @NotNull
  List<HaskellQual> getQualList();

  @Nullable
  HaskellQvar getQvar();

  @Nullable
  HaskellStmt getStmt();

  @Nullable
  PsiElement getChartoken();

  @Nullable
  PsiElement getDoubleperiod();

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
  PsiElement getPipe();

  @Nullable
  PsiElement getRbrace();

  @Nullable
  PsiElement getRbracket();

  @Nullable
  PsiElement getRparen();

}
