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

public class HaskellQvaropImpl extends HaskellCompositeElementImpl implements HaskellQvarop {

  public HaskellQvaropImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitQvarop(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellQvarid getQvarid() {
    return PsiTreeUtil.getChildOfType(this, HaskellQvarid.class);
  }

  @Override
  @Nullable
  public HaskellQvarsym getQvarsym() {
    return PsiTreeUtil.getChildOfType(this, HaskellQvarsym.class);
  }

}
