// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellFunorpatdecl extends PsiElement {

  @NotNull
  List<HaskellPat> getPatList();

  @NotNull
  List<HaskellPstringtoken> getPstringtokenList();

  @NotNull
  List<HaskellQcon> getQconList();

  @NotNull
  List<HaskellQvar> getQvarList();

  @NotNull
  HaskellRhs getRhs();

  @NotNull
  List<HaskellVarid> getVaridList();

  @Nullable
  HaskellVarop getVarop();

  @NotNull
  List<HaskellVarsym> getVarsymList();

}
