// This is a generated file. Not intended for manual editing.
package com.haskforce.psi;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.psi.PsiElement;

public interface HaskellStmts extends PsiElement {

  @NotNull
  List<HaskellExp> getExpList();

  @NotNull
  List<HaskellFunorpatdecl> getFunorpatdeclList();

  @NotNull
  List<HaskellGendecl> getGendeclList();

  @NotNull
  List<HaskellPat> getPatList();

  @NotNull
  List<HaskellPpragma> getPpragmaList();

}
