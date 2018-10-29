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

public class HaskellDerivingdeclImpl extends HaskellCompositeElementImpl implements HaskellDerivingdecl {

  public HaskellDerivingdeclImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitDerivingdecl(this);
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
  public HaskellPpragma getPpragma() {
    return PsiTreeUtil.getChildOfType(this, HaskellPpragma.class);
  }

  @Override
  @NotNull
  public PsiElement getDeriving() {
    return notNullChild(findChildByType(DERIVING));
  }

  @Override
  @Nullable
  public PsiElement getInstance() {
    return findChildByType(INSTANCE);
  }

  @Override
  @Nullable
  public PsiElement getNewtype() {
    return findChildByType(NEWTYPE);
  }

}
