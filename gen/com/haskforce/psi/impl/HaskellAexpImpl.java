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

public class HaskellAexpImpl extends ASTWrapperPsiElement implements HaskellAexp {

  public HaskellAexpImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitAexp(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellAexp> getAexpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellAexp.class);
  }

  @Override
  @Nullable
  public HaskellExp getExp() {
    return findChildByClass(HaskellExp.class);
  }

  @Override
  @Nullable
  public HaskellFunlhs getFunlhs() {
    return findChildByClass(HaskellFunlhs.class);
  }

  @Override
  @Nullable
  public HaskellGcon getGcon() {
    return findChildByClass(HaskellGcon.class);
  }

  @Override
  @Nullable
  public HaskellInfixexp getInfixexp() {
    return findChildByClass(HaskellInfixexp.class);
  }

  @Override
  @Nullable
  public HaskellQcon getQcon() {
    return findChildByClass(HaskellQcon.class);
  }

  @Override
  @NotNull
  public List<HaskellQop> getQopList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQop.class);
  }

  @Override
  @NotNull
  public List<HaskellQual> getQualList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQual.class);
  }

  @Override
  @Nullable
  public HaskellQvar getQvar() {
    return findChildByClass(HaskellQvar.class);
  }

  @Override
  @Nullable
  public HaskellStmt getStmt() {
    return findChildByClass(HaskellStmt.class);
  }

  @Override
  @Nullable
  public PsiElement getChartoken() {
    return findChildByType(CHARTOKEN);
  }

  @Override
  @Nullable
  public PsiElement getDoubleperiod() {
    return findChildByType(DOUBLEPERIOD);
  }

  @Override
  @Nullable
  public PsiElement getFloattoken() {
    return findChildByType(FLOATTOKEN);
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
  public PsiElement getPipe() {
    return findChildByType(PIPE);
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
  public PsiElement getRparen() {
    return findChildByType(RPAREN);
  }

  @Override
  @Nullable
  public PsiElement getStringtoken() {
    return findChildByType(STRINGTOKEN);
  }

}
