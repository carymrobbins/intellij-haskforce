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

public class HaskellForeigndeclImpl extends HaskellCompositeElementImpl implements HaskellForeigndecl {

  public HaskellForeigndeclImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitForeigndecl(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellAtype> getAtypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellAtype.class);
  }

  @Override
  @Nullable
  public HaskellClscontext getClscontext() {
    return PsiTreeUtil.getChildOfType(this, HaskellClscontext.class);
  }

  @Override
  @Nullable
  public HaskellPstringtoken getPstringtoken() {
    return PsiTreeUtil.getChildOfType(this, HaskellPstringtoken.class);
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
  public PsiElement getExporttoken() {
    return findChildByType(EXPORTTOKEN);
  }

  @Override
  @NotNull
  public PsiElement getForeign() {
    return notNullChild(findChildByType(FOREIGN));
  }

  @Override
  @Nullable
  public PsiElement getImport() {
    return findChildByType(IMPORT);
  }

  @Override
  @Nullable
  public PsiElement getType() {
    return findChildByType(TYPE);
  }

  @Override
  @Nullable
  public PsiElement getDoublearrow() {
    return findChildByType(DOUBLEARROW);
  }

  @Override
  @Nullable
  public PsiElement getDoublecolon() {
    return findChildByType(DOUBLECOLON);
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
