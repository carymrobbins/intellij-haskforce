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

public class HaskellExportImpl extends HaskellCompositeElementImpl implements HaskellExport {

  public HaskellExportImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitExport(this);
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
    return findChildByClass(HaskellQconid.class);
  }

  @Override
  @Nullable
  public HaskellQtycon getQtycon() {
    return findChildByClass(HaskellQtycon.class);
  }

  @Override
  @Nullable
  public HaskellQvar getQvar() {
    return findChildByClass(HaskellQvar.class);
  }

  @Override
  @Nullable
  public HaskellQvars getQvars() {
    return findChildByClass(HaskellQvars.class);
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
