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

public class HaskellBkindImpl extends HaskellCompositeElementImpl implements HaskellBkind {

  public HaskellBkindImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitBkind(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public HaskellAkind getAkind() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, HaskellAkind.class));
  }

  @Override
  @Nullable
  public HaskellBkind getBkind() {
    return PsiTreeUtil.getChildOfType(this, HaskellBkind.class);
  }

}
