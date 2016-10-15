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

public class HaskellPatImpl extends HaskellCompositeElementImpl implements HaskellPat {

  public HaskellPatImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitPat(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellConid> getConidList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellConid.class);
  }

  @Override
  @Nullable
  public HaskellCtype getCtype() {
    return PsiTreeUtil.getChildOfType(this, HaskellCtype.class);
  }

  @Override
  @NotNull
  public List<HaskellExp> getExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellExp.class);
  }

  @Override
  @NotNull
  public List<HaskellPat> getPatList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPat.class);
  }

  @Override
  @NotNull
  public List<HaskellPstringtoken> getPstringtokenList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPstringtoken.class);
  }

  @Override
  @NotNull
  public List<HaskellQcon> getQconList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQcon.class);
  }

  @Override
  @Nullable
  public HaskellQconop getQconop() {
    return PsiTreeUtil.getChildOfType(this, HaskellQconop.class);
  }

  @Override
  @NotNull
  public List<HaskellQvar> getQvarList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQvar.class);
  }

  @Override
  @NotNull
  public List<HaskellVarid> getVaridList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellVarid.class);
  }

  @Override
  @NotNull
  public List<HaskellVarsym> getVarsymList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellVarsym.class);
  }

  @Override
  @Nullable
  public PsiElement getDoublecolon() {
    return findChildByType(DOUBLECOLON);
  }

  @Override
  @Nullable
  public PsiElement getDoubleperiod() {
    return findChildByType(DOUBLEPERIOD);
  }

  @Override
  @Nullable
  public PsiElement getMinus() {
    return findChildByType(MINUS);
  }

}
