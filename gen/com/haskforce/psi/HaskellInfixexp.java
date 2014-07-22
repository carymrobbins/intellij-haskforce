// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellInfixexp extends PsiElement {

  @Nullable
  HaskellAlts getAlts();

  @Nullable
  HaskellExp getExp();

  @Nullable
  HaskellGendecl getGendecl();

  @NotNull
  List<HaskellInfixexp> getInfixexpList();

  @NotNull
  List<HaskellLpat> getLpatList();

  @Nullable
  HaskellPpragma getPpragma();

  @NotNull
  List<HaskellPstringtoken> getPstringtokenList();

  @NotNull
  List<HaskellQcon> getQconList();

  @NotNull
  List<HaskellQconop> getQconopList();

  @NotNull
  List<HaskellQop> getQopList();

  @NotNull
  List<HaskellQvar> getQvarList();

  @Nullable
  HaskellRhs getRhs();

  @Nullable
  HaskellStmts getStmts();

  @NotNull
  List<HaskellVarid> getVaridList();

  @Nullable
  HaskellVarop getVarop();

  @NotNull
  List<HaskellVarsym> getVarsymList();

  @Nullable
  PsiElement getBackslash();

  @Nullable
  PsiElement getDoubleperiod();

  @Nullable
  PsiElement getMinus();

  @Nullable
  PsiElement getPipe();

  @Nullable
  PsiElement getRightarrow();

  @Nullable
  PsiElement getSemicolon();

}
