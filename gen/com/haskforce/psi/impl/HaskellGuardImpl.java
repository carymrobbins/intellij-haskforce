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

public class HaskellGuardImpl extends HaskellCompositeElementImpl implements HaskellGuard {

  public HaskellGuardImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitGuard(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellAlt> getAltList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellAlt.class);
  }

  @Override
  @NotNull
  public List<HaskellCtype> getCtypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellCtype.class);
  }

  @Override
  @Nullable
  public HaskellExp getExp() {
    return PsiTreeUtil.getChildOfType(this, HaskellExp.class);
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
  public List<HaskellLabel> getLabelList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellLabel.class);
  }

  @Override
  @NotNull
  public List<HaskellLetexp> getLetexpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellLetexp.class);
  }

  @Override
  @NotNull
  public List<HaskellListlike> getListlikeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellListlike.class);
  }

  @Override
  @Nullable
  public HaskellPat getPat() {
    return PsiTreeUtil.getChildOfType(this, HaskellPat.class);
  }

  @Override
  @NotNull
  public List<HaskellPpragma> getPpragmaList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPpragma.class);
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
  public List<HaskellQop> getQopList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQop.class);
  }

  @Override
  @NotNull
  public List<HaskellQqblob> getQqblobList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQqblob.class);
  }

  @Override
  @NotNull
  public List<HaskellQvar> getQvarList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQvar.class);
  }

  @Override
  @NotNull
  public List<HaskellQvarid> getQvaridList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQvarid.class);
  }

  @Override
  @NotNull
  public List<HaskellStmts> getStmtsList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellStmts.class);
  }

  @Override
  @NotNull
  public List<HaskellVarid> getVaridList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellVarid.class);
  }

  @Override
  @NotNull
  public List<HaskellVarsym> getVarsymList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellVarsym.class);
  }

  @Override
  @Nullable
  public PsiElement getLet() {
    return findChildByType(LET);
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
  public PsiElement getLeftarrow() {
    return findChildByType(LEFTARROW);
  }

  @Override
  @Nullable
  public PsiElement getRbrace() {
    return findChildByType(RBRACE);
  }

}
