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

public class HaskellChartokenImpl extends ASTWrapperPsiElement implements HaskellChartoken {

  public HaskellChartokenImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitChartoken(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellEscape getEscape() {
    return findChildByClass(HaskellEscape.class);
  }

  @Override
  @Nullable
  public HaskellGraphic getGraphic() {
    return findChildByClass(HaskellGraphic.class);
  }

  @Override
  @Nullable
  public PsiElement getSpace() {
    return findChildByType(SPACE);
  }

}
