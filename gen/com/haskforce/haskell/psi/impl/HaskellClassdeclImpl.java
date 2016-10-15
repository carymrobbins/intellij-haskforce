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

public class HaskellClassdeclImpl extends HaskellCompositeElementImpl implements HaskellClassdecl {

  public HaskellClassdeclImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitClassdecl(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellCdecl getCdecl() {
    return PsiTreeUtil.getChildOfType(this, HaskellCdecl.class);
  }

  @Override
  @Nullable
  public HaskellCtype getCtype() {
    return PsiTreeUtil.getChildOfType(this, HaskellCtype.class);
  }

  @Override
  @NotNull
  public List<HaskellPpragma> getPpragmaList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPpragma.class);
  }

  @Override
  @NotNull
  public List<HaskellTyvar> getTyvarList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellTyvar.class);
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
  @NotNull
  public PsiElement getClasstoken() {
    return notNullChild(findChildByType(CLASSTOKEN));
  }

  @Override
  @Nullable
  public PsiElement getLbrace() {
    return findChildByType(LBRACE);
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

}
