// This is a generated file. Not intended for manual editing.
package com.haskforce.haskell.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.haskforce.haskell.psi.HaskellTypes.*;
import com.haskforce.haskell.psi.*;

public class HaskellStmtsImpl extends HaskellCompositeElementImpl implements HaskellStmts {

  public HaskellStmtsImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitStmts(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellExp> getExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellExp.class);
  }

  @Override
  @NotNull
  public List<HaskellFunorpatdecl> getFunorpatdeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellFunorpatdecl.class);
  }

  @Override
  @NotNull
  public List<HaskellGendecl> getGendeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellGendecl.class);
  }

  @Override
  @Nullable
  public HaskellLetexp getLetexp() {
    return PsiTreeUtil.getChildOfType(this, HaskellLetexp.class);
  }

  @Override
  @NotNull
  public List<HaskellPat> getPatList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPat.class);
  }

  @Override
  @NotNull
  public List<HaskellPpragma> getPpragmaList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPpragma.class);
  }

  @Override
  @NotNull
  public List<HaskellStmts> getStmtsList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellStmts.class);
  }

}
