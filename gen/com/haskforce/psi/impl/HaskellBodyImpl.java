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

public class HaskellBodyImpl extends HaskellCompositeElementImpl implements HaskellBody {

  public HaskellBodyImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitBody(this);
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
  @Nullable
  public HaskellClassdecl getClassdecl() {
    return PsiTreeUtil.getChildOfType(this, HaskellClassdecl.class);
  }

  @Override
  @NotNull
  public List<HaskellCtype> getCtypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellCtype.class);
  }

  @Override
  @Nullable
  public HaskellDatadecl getDatadecl() {
    return PsiTreeUtil.getChildOfType(this, HaskellDatadecl.class);
  }

  @Override
  @Nullable
  public HaskellDefaultdecl getDefaultdecl() {
    return PsiTreeUtil.getChildOfType(this, HaskellDefaultdecl.class);
  }

  @Override
  @Nullable
  public HaskellDerivingdecl getDerivingdecl() {
    return PsiTreeUtil.getChildOfType(this, HaskellDerivingdecl.class);
  }

  @Override
  @Nullable
  public HaskellExp getExp() {
    return PsiTreeUtil.getChildOfType(this, HaskellExp.class);
  }

  @Override
  @Nullable
  public HaskellForeigndecl getForeigndecl() {
    return PsiTreeUtil.getChildOfType(this, HaskellForeigndecl.class);
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
  public List<HaskellImpdecl> getImpdeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellImpdecl.class);
  }

  @Override
  @Nullable
  public HaskellInstancedecl getInstancedecl() {
    return PsiTreeUtil.getChildOfType(this, HaskellInstancedecl.class);
  }

  @Override
  @NotNull
  public List<HaskellLabel> getLabelList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellLabel.class);
  }

  @Override
  @Nullable
  public HaskellLetexp getLetexp() {
    return PsiTreeUtil.getChildOfType(this, HaskellLetexp.class);
  }

  @Override
  @NotNull
  public List<HaskellListlike> getListlikeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellListlike.class);
  }

  @Override
  @Nullable
  public HaskellNewtypedecl getNewtypedecl() {
    return PsiTreeUtil.getChildOfType(this, HaskellNewtypedecl.class);
  }

  @Override
  @NotNull
  public List<HaskellPat> getPatList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPat.class);
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
  @Nullable
  public HaskellStmts getStmts() {
    return PsiTreeUtil.getChildOfType(this, HaskellStmts.class);
  }

  @Override
  @Nullable
  public HaskellTypedecl getTypedecl() {
    return PsiTreeUtil.getChildOfType(this, HaskellTypedecl.class);
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
  public PsiElement getCase() {
    return findChildByType(CASE);
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
  public PsiElement getIf() {
    return findChildByType(IF);
  }

  @Override
  @Nullable
  public PsiElement getLcasetok() {
    return findChildByType(LCASETOK);
  }

  @Override
  @Nullable
  public PsiElement getMdotok() {
    return findChildByType(MDOTOK);
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
  public PsiElement getBackslash() {
    return findChildByType(BACKSLASH);
  }

}
