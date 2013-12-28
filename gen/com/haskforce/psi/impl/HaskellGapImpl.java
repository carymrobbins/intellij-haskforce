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

public class HaskellGapImpl extends ASTWrapperPsiElement implements HaskellGap {

  public HaskellGapImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitGap(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellWhitechar> getWhitecharList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellWhitechar.class);
  }

}
