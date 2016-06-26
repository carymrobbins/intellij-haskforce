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

public class HaskellAltImpl extends HaskellCompositeElementImpl implements HaskellAlt {

  public HaskellAltImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitAlt(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellExp> getExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellExp.class);
  }

  @Override
  @Nullable
  public HaskellFunorpatdecl getFunorpatdecl() {
    return PsiTreeUtil.getChildOfType(this, HaskellFunorpatdecl.class);
  }

  @Override
  @Nullable
  public HaskellGendecl getGendecl() {
    return PsiTreeUtil.getChildOfType(this, HaskellGendecl.class);
  }

  @Override
  @NotNull
  public List<HaskellGuard> getGuardList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellGuard.class);
  }

  @Override
  @NotNull
  public HaskellPat getPat() {
    return notNullChild(PsiTreeUtil.getChildOfType(this, HaskellPat.class));
  }

  @Override
  @NotNull
  public List<HaskellPpragma> getPpragmaList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPpragma.class);
  }

  @Override
  @Nullable
  public PsiElement getWhere() {
    return findChildByType(WHERE);
  }

  @Override
  @Nullable
  public PsiElement getWhitespacelbracetok() {
    return findChildByType(WHITESPACELBRACETOK);
  }

  @Override
  @Nullable
  public PsiElement getWhitespacerbracetok() {
    return findChildByType(WHITESPACERBRACETOK);
  }

  @Override
  @Nullable
  public PsiElement getLbrace() {
    return findChildByType(LBRACE);
  }

  @Override
  @Nullable
  public PsiElement getRbrace() {
    return findChildByType(RBRACE);
  }

}
