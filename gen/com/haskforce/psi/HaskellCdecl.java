// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellCdecl extends HaskellCompositeElement {

  @NotNull
  List<HaskellCtype> getCtypeList();

  @NotNull
  List<HaskellExp> getExpList();

  @Nullable
  HaskellGendecl getGendecl();

  @Nullable
  HaskellKind getKind();

  @NotNull
  List<HaskellPat> getPatList();

  @Nullable
  HaskellPpragma getPpragma();

  @NotNull
  List<HaskellPstringtoken> getPstringtokenList();

  @NotNull
  List<HaskellQcon> getQconList();

  @NotNull
  List<HaskellQvar> getQvarList();

  @Nullable
  HaskellRhs getRhs();

  @Nullable
  HaskellTypee getTypee();

  @NotNull
  List<HaskellVarid> getVaridList();

  @Nullable
  HaskellVarop getVarop();

  @NotNull
  List<HaskellVarsym> getVarsymList();

  @Nullable
  PsiElement getData();

  @Nullable
  PsiElement getDefault();

  @Nullable
  PsiElement getInstance();

  @Nullable
  PsiElement getType();

  @Nullable
  PsiElement getDoublecolon();

  @Nullable
  PsiElement getEquals();

}
