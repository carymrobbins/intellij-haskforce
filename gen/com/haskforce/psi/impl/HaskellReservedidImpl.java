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

public class HaskellReservedidImpl extends ASTWrapperPsiElement implements HaskellReservedid {

  public HaskellReservedidImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitReservedid(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public PsiElement getCase() {
    return findChildByType(CASE);
  }

  @Override
  @Nullable
  public PsiElement getClasstoken() {
    return findChildByType(CLASSTOKEN);
  }

  @Override
  @Nullable
  public PsiElement getData() {
    return findChildByType(DATA);
  }

  @Override
  @Nullable
  public PsiElement getDefault() {
    return findChildByType(DEFAULT);
  }

  @Override
  @Nullable
  public PsiElement getDeriving() {
    return findChildByType(DERIVING);
  }

  @Override
  @Nullable
  public PsiElement getDo() {
    return findChildByType(DO);
  }

  @Override
  @Nullable
  public PsiElement getElse() {
    return findChildByType(ELSE);
  }

  @Override
  @Nullable
  public PsiElement getForeign() {
    return findChildByType(FOREIGN);
  }

  @Override
  @Nullable
  public PsiElement getIf() {
    return findChildByType(IF);
  }

  @Override
  @Nullable
  public PsiElement getImport() {
    return findChildByType(IMPORT);
  }

  @Override
  @Nullable
  public PsiElement getIn() {
    return findChildByType(IN);
  }

  @Override
  @Nullable
  public PsiElement getInfix() {
    return findChildByType(INFIX);
  }

  @Override
  @Nullable
  public PsiElement getInfixl() {
    return findChildByType(INFIXL);
  }

  @Override
  @Nullable
  public PsiElement getInfixr() {
    return findChildByType(INFIXR);
  }

  @Override
  @Nullable
  public PsiElement getInstance() {
    return findChildByType(INSTANCE);
  }

  @Override
  @Nullable
  public PsiElement getLet() {
    return findChildByType(LET);
  }

  @Override
  @Nullable
  public PsiElement getModule() {
    return findChildByType(MODULE);
  }

  @Override
  @Nullable
  public PsiElement getNewtype() {
    return findChildByType(NEWTYPE);
  }

  @Override
  @Nullable
  public PsiElement getOf() {
    return findChildByType(OF);
  }

  @Override
  @Nullable
  public PsiElement getThen() {
    return findChildByType(THEN);
  }

  @Override
  @Nullable
  public PsiElement getType() {
    return findChildByType(TYPE);
  }

  @Override
  @Nullable
  public PsiElement getWhere() {
    return findChildByType(WHERE);
  }

}
