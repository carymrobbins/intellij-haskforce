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

public class HaskellTypeeImpl extends HaskellCompositeElementImpl implements HaskellTypee {

  public HaskellTypeeImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitTypee(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellAtype> getAtypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellAtype.class);
  }

  @Override
  @Nullable
  public HaskellQconop getQconop() {
    return PsiTreeUtil.getChildOfType(this, HaskellQconop.class);
  }

  @Override
  @Nullable
  public HaskellQtyconop getQtyconop() {
    return PsiTreeUtil.getChildOfType(this, HaskellQtyconop.class);
  }

  @Override
  @Nullable
  public HaskellTypee getTypee() {
    return PsiTreeUtil.getChildOfType(this, HaskellTypee.class);
  }

  @Override
  @Nullable
  public HaskellVarop getVarop() {
    return PsiTreeUtil.getChildOfType(this, HaskellVarop.class);
  }

  @Override
  @Nullable
  public PsiElement getRightarrow() {
    return findChildByType(RIGHTARROW);
  }

  @Override
  @Nullable
  public PsiElement getSinglequote() {
    return findChildByType(SINGLEQUOTE);
  }

}
