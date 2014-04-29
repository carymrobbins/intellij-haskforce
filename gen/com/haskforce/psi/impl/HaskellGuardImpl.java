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

public class HaskellGuardImpl extends ASTWrapperPsiElement implements HaskellGuard {

  public HaskellGuardImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitGuard(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellInfixexp getInfixexp() {
    return findChildByClass(HaskellInfixexp.class);
  }

  @Override
  @NotNull
  public List<HaskellNcomment> getNcommentList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellNcomment.class);
  }

  @Override
  @Nullable
  public HaskellPat getPat() {
    return findChildByClass(HaskellPat.class);
  }

  @Override
  @NotNull
  public List<HaskellPragma> getPragmaList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPragma.class);
  }

  @Override
  @NotNull
  public List<HaskellQconid> getQconidList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQconid.class);
  }

  @Override
  @NotNull
  public List<HaskellQconsym> getQconsymList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQconsym.class);
  }

  @Override
  @NotNull
  public List<HaskellQinfixconid> getQinfixconidList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQinfixconid.class);
  }

  @Override
  @NotNull
  public List<HaskellQinfixvarid> getQinfixvaridList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQinfixvarid.class);
  }

  @Override
  @NotNull
  public List<HaskellQvarid> getQvaridList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQvarid.class);
  }

  @Override
  @NotNull
  public List<HaskellQvarsym> getQvarsymList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQvarsym.class);
  }

  @Override
  @NotNull
  public List<HaskellReservedop> getReservedopList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellReservedop.class);
  }

  @Override
  @NotNull
  public List<HaskellSpecial> getSpecialList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellSpecial.class);
  }

  @Override
  @NotNull
  public List<HaskellWhitechar> getWhitecharList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellWhitechar.class);
  }

  @Override
  @Nullable
  public PsiElement getLeftarrow() {
    return findChildByType(LEFTARROW);
  }

}