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

public class HaskellLabelImpl extends HaskellCompositeElementImpl implements HaskellLabel {

  public HaskellLabelImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitLabel(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public PsiElement getLabelRegexp() {
    return notNullChild(findChildByType(LABELREGEXP));
  }

}
