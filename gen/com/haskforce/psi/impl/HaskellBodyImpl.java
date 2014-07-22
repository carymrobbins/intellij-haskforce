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

public class HaskellBodyImpl extends ASTWrapperPsiElement implements HaskellBody {

  public HaskellBodyImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitBody(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellClassdecl getClassdecl() {
    return findChildByClass(HaskellClassdecl.class);
  }

  @Override
  @NotNull
  public List<HaskellCpp> getCppList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellCpp.class);
  }

  @Override
  @Nullable
  public HaskellDatadecl getDatadecl() {
    return findChildByClass(HaskellDatadecl.class);
  }

  @Override
  @Nullable
  public HaskellDefaultdecl getDefaultdecl() {
    return findChildByClass(HaskellDefaultdecl.class);
  }

  @Override
  @Nullable
  public HaskellDerivingdecl getDerivingdecl() {
    return findChildByClass(HaskellDerivingdecl.class);
  }

  @Override
  @Nullable
  public HaskellForeigndecl getForeigndecl() {
    return findChildByClass(HaskellForeigndecl.class);
  }

  @Override
  @Nullable
  public HaskellFunorpatdecl getFunorpatdecl() {
    return findChildByClass(HaskellFunorpatdecl.class);
  }

  @Override
  @Nullable
  public HaskellGendecl getGendecl() {
    return findChildByClass(HaskellGendecl.class);
  }

  @Override
  @Nullable
  public HaskellImpdecl getImpdecl() {
    return findChildByClass(HaskellImpdecl.class);
  }

  @Override
  @Nullable
  public HaskellInstancedecl getInstancedecl() {
    return findChildByClass(HaskellInstancedecl.class);
  }

  @Override
  @Nullable
  public HaskellNewtypedecl getNewtypedecl() {
    return findChildByClass(HaskellNewtypedecl.class);
  }

  @Override
  @NotNull
  public List<HaskellPpragma> getPpragmaList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPpragma.class);
  }

  @Override
  @Nullable
  public HaskellTypedecl getTypedecl() {
    return findChildByClass(HaskellTypedecl.class);
  }

  @Override
  @Nullable
  public PsiElement getLbrace() {
    return findChildByType(LBRACE);
  }

  @Override
  @Nullable
  public PsiElement getRbrace() {
    return findChildByType(RBRACE);
  }

}
