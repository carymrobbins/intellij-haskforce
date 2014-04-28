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

public class HaskellExpImpl extends ASTWrapperPsiElement implements HaskellExp {

  public HaskellExpImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitExp(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellContext getContext() {
    return findChildByClass(HaskellContext.class);
  }

  @Override
  @NotNull
  public HaskellInfixexp getInfixexp() {
    return findNotNullChildByClass(HaskellInfixexp.class);
  }

  @Override
  @Nullable
  public HaskellTypee getTypee() {
    return findChildByClass(HaskellTypee.class);
  }

  @Override
  @Nullable
  public PsiElement getDoublearrow() {
    return findChildByType(DOUBLEARROW);
  }

  @Override
  @Nullable
  public PsiElement getDoublecolon() {
    return findChildByType(DOUBLECOLON);
  }

}
