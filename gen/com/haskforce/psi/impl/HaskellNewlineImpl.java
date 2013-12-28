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

public class HaskellNewlineImpl extends ASTWrapperPsiElement implements HaskellNewline {

  public HaskellNewlineImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitNewline(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public PsiElement getFormfeed() {
    return findChildByType(FORMFEED);
  }

  @Override
  @Nullable
  public PsiElement getLinefeed() {
    return findChildByType(LINEFEED);
  }

  @Override
  @Nullable
  public PsiElement getReturn() {
    return findChildByType(RETURN);
  }

}
