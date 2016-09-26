// This is a generated file. Not intended for manual editing.
package com.haskforce.haskell.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.haskforce.haskell.psi.HaskellTypes.*;
import com.haskforce.haskell.psi.*;

public class HaskellAkindImpl extends HaskellCompositeElementImpl implements HaskellAkind {

  public HaskellAkindImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitAkind(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellKind getKind() {
    return PsiTreeUtil.getChildOfType(this, HaskellKind.class);
  }

  @Override
  @Nullable
  public HaskellQtycon getQtycon() {
    return PsiTreeUtil.getChildOfType(this, HaskellQtycon.class);
  }

  @Override
  @Nullable
  public HaskellTyvar getTyvar() {
    return PsiTreeUtil.getChildOfType(this, HaskellTyvar.class);
  }

  @Override
  @Nullable
  public PsiElement getAsterisk() {
    return findChildByType(ASTERISK);
  }

  @Override
  @Nullable
  public PsiElement getExclamation() {
    return findChildByType(EXCLAMATION);
  }

  @Override
  @Nullable
  public PsiElement getLbracket() {
    return findChildByType(LBRACKET);
  }

  @Override
  @Nullable
  public PsiElement getLparen() {
    return findChildByType(LPAREN);
  }

  @Override
  @Nullable
  public PsiElement getRbracket() {
    return findChildByType(RBRACKET);
  }

  @Override
  @Nullable
  public PsiElement getRparen() {
    return findChildByType(RPAREN);
  }

}
