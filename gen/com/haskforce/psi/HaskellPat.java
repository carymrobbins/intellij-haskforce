// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellPat extends PsiElement {

  @Nullable
  HaskellCtype getCtype();

  @NotNull
  List<HaskellExp> getExpList();

  @NotNull
  List<HaskellPat> getPatList();

  @NotNull
  List<HaskellPstringtoken> getPstringtokenList();

  @NotNull
  List<HaskellQcon> getQconList();

  @Nullable
  HaskellQconop getQconop();

  @NotNull
  List<HaskellQvar> getQvarList();

  @NotNull
  List<HaskellVarid> getVaridList();

  @NotNull
  List<HaskellVarsym> getVarsymList();

  @Nullable
  PsiElement getDoublecolon();

  @Nullable
  PsiElement getMinus();

}
