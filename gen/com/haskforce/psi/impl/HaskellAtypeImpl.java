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

public class HaskellAtypeImpl extends HaskellCompositeElementImpl implements HaskellAtype {

  public HaskellAtypeImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitAtype(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellAtype> getAtypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellAtype.class);
  }

  @Override
  @NotNull
  public List<HaskellCtype> getCtypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellCtype.class);
  }

  @Override
  @Nullable
  public HaskellKind getKind() {
    return findChildByClass(HaskellKind.class);
  }

  @Override
  @Nullable
  public HaskellOqtycon getOqtycon() {
    return findChildByClass(HaskellOqtycon.class);
  }

  @Override
  @Nullable
  public HaskellPstringtoken getPstringtoken() {
    return findChildByClass(HaskellPstringtoken.class);
  }

  @Override
  @NotNull
  public List<HaskellTvBndr> getTvBndrList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellTvBndr.class);
  }

  @Override
  @NotNull
  public List<HaskellTypee> getTypeeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellTypee.class);
  }

  @Override
  @Nullable
  public HaskellTyvar getTyvar() {
    return findChildByClass(HaskellTyvar.class);
  }

  @Override
  @NotNull
  public List<HaskellVars> getVarsList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellVars.class);
  }

  @Override
  @Nullable
  public PsiElement getForalltoken() {
    return findChildByType(FORALLTOKEN);
  }

  @Override
  @Nullable
  public PsiElement getDoublehash() {
    return findChildByType(DOUBLEHASH);
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
