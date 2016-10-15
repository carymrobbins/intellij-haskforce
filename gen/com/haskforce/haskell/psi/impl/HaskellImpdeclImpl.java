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

public class HaskellImpdeclImpl extends HaskellCompositeElementImpl implements HaskellImpdecl {

  public HaskellImpdeclImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitImpdecl(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellImpempty getImpempty() {
    return PsiTreeUtil.getChildOfType(this, HaskellImpempty.class);
  }

  @Override
  @NotNull
  public List<HaskellImportt> getImporttList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellImportt.class);
  }

  @Override
  @NotNull
  public List<HaskellQconid> getQconidList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQconid.class);
  }

  @Override
  @Nullable
  public PsiElement getAs() {
    return findChildByType(AS);
  }

  @Override
  @Nullable
  public PsiElement getHiding() {
    return findChildByType(HIDING);
  }

  @Override
  @NotNull
  public PsiElement getImport() {
    return notNullChild(findChildByType(IMPORT));
  }

  @Override
  @Nullable
  public PsiElement getQualified() {
    return findChildByType(QUALIFIED);
  }

  @Override
  @Nullable
  public PsiElement getLparen() {
    return findChildByType(LPAREN);
  }

  @Override
  @Nullable
  public PsiElement getRparen() {
    return findChildByType(RPAREN);
  }

}
