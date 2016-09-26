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

public class HaskellGendeclImpl extends HaskellCompositeElementImpl implements HaskellGendecl {

  public HaskellGendeclImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitGendecl(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellCtype getCtype() {
    return PsiTreeUtil.getChildOfType(this, HaskellCtype.class);
  }

  @Override
  @Nullable
  public HaskellFixity getFixity() {
    return PsiTreeUtil.getChildOfType(this, HaskellFixity.class);
  }

  @Override
  @NotNull
  public List<HaskellOp> getOpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellOp.class);
  }

  @Override
  @Nullable
  public HaskellVars getVars() {
    return PsiTreeUtil.getChildOfType(this, HaskellVars.class);
  }

  @Override
  @Nullable
  public PsiElement getDoublecolon() {
    return findChildByType(DOUBLECOLON);
  }

  @Override
  @Nullable
  public PsiElement getIntegertoken() {
    return findChildByType(INTEGERTOKEN);
  }

}
