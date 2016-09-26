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

public class HaskellCdeclImpl extends HaskellCompositeElementImpl implements HaskellCdecl {

  public HaskellCdeclImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitCdecl(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellCtype> getCtypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellCtype.class);
  }

  @Override
  @NotNull
  public List<HaskellExp> getExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellExp.class);
  }

  @Override
  @Nullable
  public HaskellGendecl getGendecl() {
    return PsiTreeUtil.getChildOfType(this, HaskellGendecl.class);
  }

  @Override
  @Nullable
  public HaskellKind getKind() {
    return PsiTreeUtil.getChildOfType(this, HaskellKind.class);
  }

  @Override
  @NotNull
  public List<HaskellPat> getPatList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPat.class);
  }

  @Override
  @Nullable
  public HaskellPpragma getPpragma() {
    return PsiTreeUtil.getChildOfType(this, HaskellPpragma.class);
  }

  @Override
  @NotNull
  public List<HaskellPstringtoken> getPstringtokenList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPstringtoken.class);
  }

  @Override
  @NotNull
  public List<HaskellQcon> getQconList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQcon.class);
  }

  @Override
  @NotNull
  public List<HaskellQvar> getQvarList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQvar.class);
  }

  @Override
  @Nullable
  public HaskellRhs getRhs() {
    return PsiTreeUtil.getChildOfType(this, HaskellRhs.class);
  }

  @Override
  @Nullable
  public HaskellTypee getTypee() {
    return PsiTreeUtil.getChildOfType(this, HaskellTypee.class);
  }

  @Override
  @NotNull
  public List<HaskellVarid> getVaridList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellVarid.class);
  }

  @Override
  @Nullable
  public HaskellVarop getVarop() {
    return PsiTreeUtil.getChildOfType(this, HaskellVarop.class);
  }

  @Override
  @NotNull
  public List<HaskellVarsym> getVarsymList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellVarsym.class);
  }

  @Override
  @Nullable
  public PsiElement getData() {
    return findChildByType(DATA);
  }

  @Override
  @Nullable
  public PsiElement getInstance() {
    return findChildByType(INSTANCE);
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
  public PsiElement getEquals() {
    return findChildByType(EQUALS);
  }

}
