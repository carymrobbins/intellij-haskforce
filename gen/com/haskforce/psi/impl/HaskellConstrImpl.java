// This is a generated file. Not intended for manual editing.
package com.haskforce.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.haskforce.psi.HaskellTypes.*;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.haskforce.psi.*;

public class HaskellConstrImpl extends ASTWrapperPsiElement implements HaskellConstr {

  public HaskellConstrImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitConstr(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellAtype> getAtypeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellAtype.class);
  }

  @Override
  @Nullable
  public HaskellCon getCon() {
    return findChildByClass(HaskellCon.class);
  }

  @Override
  @Nullable
  public HaskellConop getConop() {
    return findChildByClass(HaskellConop.class);
  }

  @Override
  @NotNull
  public List<HaskellTypee> getTypeeList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellTypee.class);
  }

  @Override
  @NotNull
  public List<HaskellVars> getVarsList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellVars.class);
  }

}
