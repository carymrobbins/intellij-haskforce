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

public class HaskellImporttImpl extends HaskellCompositeElementImpl implements HaskellImportt {

  public HaskellImporttImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitImportt(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellCon> getConList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellCon.class);
  }

  @Override
  @Nullable
  public HaskellTycon getTycon() {
    return PsiTreeUtil.getChildOfType(this, HaskellTycon.class);
  }

  @Override
  @NotNull
  public List<HaskellVarid> getVaridList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellVarid.class);
  }

  @Override
  @Nullable
  public HaskellVars getVars() {
    return PsiTreeUtil.getChildOfType(this, HaskellVars.class);
  }

  @Override
  @NotNull
  public List<HaskellVarsym> getVarsymList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellVarsym.class);
  }

  @Override
  @Nullable
  public PsiElement getDoubleperiod() {
    return findChildByType(DOUBLEPERIOD);
  }

}
