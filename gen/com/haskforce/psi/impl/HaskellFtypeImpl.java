// This is a generated file. Not intended for manual editing.
package com.haskforce.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.haskforce.psi.HaskellTypes.*;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.haskforce.psi.*;

public class HaskellFtypeImpl extends ASTWrapperPsiElement implements HaskellFtype {

  public HaskellFtypeImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitFtype(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellFatype getFatype() {
    return findChildByClass(HaskellFatype.class);
  }

  @Override
  @Nullable
  public HaskellFrtype getFrtype() {
    return findChildByClass(HaskellFrtype.class);
  }

  @Override
  @Nullable
  public HaskellFtype getFtype() {
    return findChildByClass(HaskellFtype.class);
  }

  @Override
  @Nullable
  public PsiElement getRightarrow() {
    return findChildByType(RIGHTARROW);
  }

}
