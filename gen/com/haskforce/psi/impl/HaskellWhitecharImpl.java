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

public class HaskellWhitecharImpl extends ASTWrapperPsiElement implements HaskellWhitechar {

  public HaskellWhitecharImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitWhitechar(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellNewline getNewline() {
    return findChildByClass(HaskellNewline.class);
  }

  @Override
  @Nullable
  public PsiElement getSpace() {
    return findChildByType(SPACE);
  }

  @Override
  @Nullable
  public PsiElement getTab() {
    return findChildByType(TAB);
  }

  @Override
  @Nullable
  public PsiElement getVertab() {
    return findChildByType(VERTAB);
  }

}
