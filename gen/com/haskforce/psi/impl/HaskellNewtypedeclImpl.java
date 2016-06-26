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

public class HaskellNewtypedeclImpl extends HaskellCompositeElementImpl implements HaskellNewtypedecl {

  public HaskellNewtypedeclImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitNewtypedecl(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellClscontext getClscontext() {
    return PsiTreeUtil.getChildOfType(this, HaskellClscontext.class);
  }

  @Override
  @Nullable
  public HaskellNewconstr getNewconstr() {
    return PsiTreeUtil.getChildOfType(this, HaskellNewconstr.class);
  }

  @Override
  @NotNull
  public List<HaskellQtycls> getQtyclsList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQtycls.class);
  }

  @Override
  @Nullable
  public HaskellTycon getTycon() {
    return PsiTreeUtil.getChildOfType(this, HaskellTycon.class);
  }

  @Override
  @NotNull
  public List<HaskellTyvar> getTyvarList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellTyvar.class);
  }

  @Override
  @Nullable
  public PsiElement getDeriving() {
    return findChildByType(DERIVING);
  }

  @Override
  @NotNull
  public PsiElement getNewtype() {
    return notNullChild(findChildByType(NEWTYPE));
  }

  @Override
  @Nullable
  public PsiElement getDoublearrow() {
    return findChildByType(DOUBLEARROW);
  }

  @Override
  @Nullable
  public PsiElement getEquals() {
    return findChildByType(EQUALS);
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
