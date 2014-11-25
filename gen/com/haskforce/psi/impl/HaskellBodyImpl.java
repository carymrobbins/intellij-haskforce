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

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitBody(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellAlt> getAltList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellAlt.class);
  }

  @Override
  @NotNull
  public List<HaskellClassdecl> getClassdeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellClassdecl.class);
  }

  @Override
  @NotNull
  public List<HaskellDatadecl> getDatadeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellDatadecl.class);
  }

  @Override
  @NotNull
  public List<HaskellDefaultdecl> getDefaultdeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellDefaultdecl.class);
  }

  @Override
  @NotNull
  public List<HaskellDerivingdecl> getDerivingdeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellDerivingdecl.class);
  }

  @Override
  @NotNull
  public List<HaskellExp> getExpList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellExp.class);
  }

  @Override
  @NotNull
  public List<HaskellForeigndecl> getForeigndeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellForeigndecl.class);
  }

  @Override
  @NotNull
  public List<HaskellFunorpatdecl> getFunorpatdeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellFunorpatdecl.class);
  }

  @Override
  @NotNull
  public List<HaskellGendecl> getGendeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellGendecl.class);
  }

  @Override
  @NotNull
  public List<HaskellImpdecl> getImpdeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellImpdecl.class);
  }

  @Override
  @NotNull
  public List<HaskellInstancedecl> getInstancedeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellInstancedecl.class);
  }

  @Override
  @NotNull
  public List<HaskellNewtypedecl> getNewtypedeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellNewtypedecl.class);
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
  public List<HaskellTypedecl> getTypedeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellTypedecl.class);
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

}
