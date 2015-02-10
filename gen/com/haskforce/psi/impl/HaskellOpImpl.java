// This is a generated file. Not intended for manual editing.
package com.haskforce.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.haskforce.psi.HaskellTypes.*;
import com.haskforce.psi.*;

public class HaskellOpImpl extends HaskellCompositeElementImpl implements HaskellOp {

  public HaskellOpImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitOp(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellConop getConop() {
    return findChildByClass(HaskellConop.class);
  }

  @Override
  @Nullable
  public HaskellVarop getVarop() {
    return findChildByClass(HaskellVarop.class);
  }

}
