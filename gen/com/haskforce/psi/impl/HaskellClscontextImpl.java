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

public class HaskellClscontextImpl extends HaskellCompositeElementImpl implements HaskellClscontext {

  public HaskellClscontextImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitClscontext(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellAtype> getAtypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellAtype.class);
  }

  @Override
  @Nullable
  public PsiElement getTilde() {
    return findChildByType(TILDE);
  }

}
