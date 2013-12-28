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

public class HaskellVaridImpl extends ASTWrapperPsiElement implements HaskellVarid {

  public HaskellVaridImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitVarid(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellDigit> getDigitList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellDigit.class);
  }

  @Override
  @NotNull
  public List<HaskellLarge> getLargeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellLarge.class);
  }

  @Override
  @NotNull
  public List<HaskellSmall> getSmallList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellSmall.class);
  }

}
