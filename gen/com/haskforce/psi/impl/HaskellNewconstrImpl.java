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

public class HaskellNewconstrImpl extends HaskellCompositeElementImpl implements HaskellNewconstr {

  public HaskellNewconstrImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitNewconstr(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellAtype getAtype() {
    return PsiTreeUtil.getChildOfType(this, HaskellAtype.class);
  }

  @Override
  @NotNull
  public HaskellCon getCon() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, HaskellCon.class));
  }

  @Override
  @Nullable
  public HaskellLabel getLabel() {
    return PsiTreeUtil.getChildOfType(this, HaskellLabel.class);
  }

  @Override
  @Nullable
  public HaskellTypee getTypee() {
    return PsiTreeUtil.getChildOfType(this, HaskellTypee.class);
  }

  @Override
  @Nullable
  public HaskellVarid getVarid() {
    return PsiTreeUtil.getChildOfType(this, HaskellVarid.class);
  }

  @Override
  @Nullable
  public HaskellVarsym getVarsym() {
    return PsiTreeUtil.getChildOfType(this, HaskellVarsym.class);
  }

  @Override
  @Nullable
  public PsiElement getType() {
    return findChildByType(TYPE);
  }

  @Override
  @Nullable
  public PsiElement getDoublecolon() {
    return findChildByType(DOUBLECOLON);
  }

  @Override
  @Nullable
  public PsiElement getLbrace() {
    return findChildByType(LBRACE);
  }

  @Override
  @Nullable
  public PsiElement getLparen() {
    return findChildByType(LPAREN);
  }

  @Override
  @Nullable
  public PsiElement getRbrace() {
    return findChildByType(RBRACE);
  }

  @Override
  @Nullable
  public PsiElement getRparen() {
    return findChildByType(RPAREN);
  }

}
