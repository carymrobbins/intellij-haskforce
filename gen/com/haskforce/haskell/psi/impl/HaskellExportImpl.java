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

public class HaskellExportImpl extends HaskellCompositeElementImpl implements HaskellExport {

  public HaskellExportImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitExport(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellCon> getConList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellCon.class);
  }

  @Override
  @Nullable
  public HaskellQconid getQconid() {
    return PsiTreeUtil.getChildOfType(this, HaskellQconid.class);
  }

  @Override
  @Nullable
  public HaskellQtycon getQtycon() {
    return PsiTreeUtil.getChildOfType(this, HaskellQtycon.class);
  }

  @Override
  @Nullable
  public HaskellQvar getQvar() {
    return PsiTreeUtil.getChildOfType(this, HaskellQvar.class);
  }

  @Override
  @Nullable
  public HaskellQvars getQvars() {
    return PsiTreeUtil.getChildOfType(this, HaskellQvars.class);
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
  public PsiElement getModuletoken() {
    return findChildByType(MODULETOKEN);
  }

  @Override
  @Nullable
  public PsiElement getDoubleperiod() {
    return findChildByType(DOUBLEPERIOD);
  }

}
