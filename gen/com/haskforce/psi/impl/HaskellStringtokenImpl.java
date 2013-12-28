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

public class HaskellStringtokenImpl extends ASTWrapperPsiElement implements HaskellStringtoken {

  public HaskellStringtokenImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitStringtoken(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellEscape> getEscapeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellEscape.class);
  }

  @Override
  @NotNull
  public List<HaskellGap> getGapList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellGap.class);
  }

  @Override
  @NotNull
  public List<HaskellGraphic> getGraphicList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellGraphic.class);
  }

}
