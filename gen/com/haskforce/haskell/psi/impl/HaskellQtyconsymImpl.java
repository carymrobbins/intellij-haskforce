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

public class HaskellQtyconsymImpl extends HaskellCompositeElementImpl implements HaskellQtyconsym {

  public HaskellQtyconsymImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitQtyconsym(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellQconsym getQconsym() {
    return PsiTreeUtil.getChildOfType(this, HaskellQconsym.class);
  }

  @Override
  @Nullable
  public HaskellQvarsym getQvarsym() {
    return PsiTreeUtil.getChildOfType(this, HaskellQvarsym.class);
  }

  @Override
  @Nullable
  public HaskellTyconsym getTyconsym() {
    return PsiTreeUtil.getChildOfType(this, HaskellTyconsym.class);
  }

}
