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

public class HaskellAtypeImpl extends HaskellCompositeElementImpl implements HaskellAtype {

  public HaskellAtypeImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitAtype(this);
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
  public List<HaskellCtype> getCtypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellCtype.class);
  }

  @Override
  @Nullable
  public HaskellKind getKind() {
    return PsiTreeUtil.getChildOfType(this, HaskellKind.class);
  }

  @Override
  @Nullable
  public HaskellOqtycon getOqtycon() {
    return PsiTreeUtil.getChildOfType(this, HaskellOqtycon.class);
  }

  @Override
  @Nullable
  public HaskellPpragma getPpragma() {
    return PsiTreeUtil.getChildOfType(this, HaskellPpragma.class);
  }

  @Override
  @Nullable
  public HaskellPstringtoken getPstringtoken() {
    return PsiTreeUtil.getChildOfType(this, HaskellPstringtoken.class);
  }

  @Override
  @NotNull
  public List<HaskellTvBndr> getTvBndrList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellTvBndr.class);
  }

  @Override
  @Nullable
  public HaskellTypee getTypee() {
    return PsiTreeUtil.getChildOfType(this, HaskellTypee.class);
  }

  @Override
  @Nullable
  public HaskellTyvar getTyvar() {
    return PsiTreeUtil.getChildOfType(this, HaskellTyvar.class);
  }

  @Override
  @Nullable
  public HaskellVars getVars() {
    return PsiTreeUtil.getChildOfType(this, HaskellVars.class);
  }

  @Override
  @Nullable
  public PsiElement getForalltoken() {
    return findChildByType(FORALLTOKEN);
  }

  @Override
  @Nullable
  public PsiElement getDoublecolon() {
    return findChildByType(DOUBLECOLON);
  }

  @Override
  @Nullable
  public PsiElement getDoublehash() {
    return findChildByType(DOUBLEHASH);
  }

  @Override
  @Nullable
  public PsiElement getExclamation() {
    return findChildByType(EXCLAMATION);
  }

  @Override
  @Nullable
  public PsiElement getHash() {
    return findChildByType(HASH);
  }

  @Override
  @Nullable
  public PsiElement getIntegertoken() {
    return findChildByType(INTEGERTOKEN);
  }

  @Override
  @Nullable
  public PsiElement getLbrace() {
    return findChildByType(LBRACE);
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
  public PsiElement getLunboxparen() {
    return findChildByType(LUNBOXPAREN);
  }

  @Override
  @Nullable
  public PsiElement getPeriod() {
    return findChildByType(PERIOD);
  }

  @Override
  @Nullable
  public PsiElement getQuestion() {
    return findChildByType(QUESTION);
  }

  @Override
  @Nullable
  public PsiElement getRbrace() {
    return findChildByType(RBRACE);
  }

  @Override
  @Nullable
  public PsiElement getRbracket() {
    return findChildByType(RBRACKET);
  }

  @Override
  @Nullable
  public PsiElement getRightarrow() {
    return findChildByType(RIGHTARROW);
  }

  @Override
  @Nullable
  public PsiElement getRparen() {
    return findChildByType(RPAREN);
  }

  @Override
  @Nullable
  public PsiElement getRunboxparen() {
    return findChildByType(RUNBOXPAREN);
  }

  @Override
  @Nullable
  public PsiElement getSinglequote() {
    return findChildByType(SINGLEQUOTE);
  }

}
