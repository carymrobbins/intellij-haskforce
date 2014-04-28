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

public class HaskellInfixexpImpl extends ASTWrapperPsiElement implements HaskellInfixexp {

  public HaskellInfixexpImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitInfixexp(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellInfixexp getInfixexp() {
    return findChildByClass(HaskellInfixexp.class);
  }

  @Override
  @Nullable
  public HaskellLexp getLexp() {
    return findChildByClass(HaskellLexp.class);
  }

  @Override
  @Nullable
  public HaskellQop getQop() {
    return findChildByClass(HaskellQop.class);
  }

  @Override
  @Nullable
  public PsiElement getMinus() {
    return findChildByType(MINUS);
  }

}
