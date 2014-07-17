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

public class HaskellPpragmaImpl extends ASTWrapperPsiElement implements HaskellPpragma {

  public HaskellPpragmaImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitPpragma(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public PsiElement getClosepragma() {
    return findChildByType(CLOSEPRAGMA);
  }

  @Override
  @NotNull
  public PsiElement getOpenpragma() {
    return findNotNullChildByType(OPENPRAGMA);
  }

}
