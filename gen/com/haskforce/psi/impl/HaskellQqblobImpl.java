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
import com.intellij.psi.impl.source.tree.injected.StringLiteralEscaper;

public class HaskellQqblobImpl extends HaskellCompositeElementImpl implements HaskellQqblob {

  public HaskellQqblobImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitQqblob(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  public boolean isValidHost() {
    return HaskellPsiImplUtil.isValidHost(this);
  }

  @Override
  public HaskellQqblob updateText(@NotNull String s) {
    return HaskellPsiImplUtil.updateText(this, s);
  }

  @Override
  @NotNull
  public StringLiteralEscaper<HaskellQqblob> createLiteralTextEscaper() {
    return HaskellPsiImplUtil.createLiteralTextEscaper(this);
  }

}
