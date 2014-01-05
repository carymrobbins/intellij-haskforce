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

public class HaskellReservedidImpl extends ASTWrapperPsiElement implements HaskellReservedid {

  public HaskellReservedidImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitReservedid(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellReservedDecl getReservedDecl() {
    return findChildByClass(HaskellReservedDecl.class);
  }

  @Override
  @Nullable
  public HaskellReservedExpr getReservedExpr() {
    return findChildByClass(HaskellReservedExpr.class);
  }

  @Override
  @Nullable
  public HaskellReservedMeta getReservedMeta() {
    return findChildByClass(HaskellReservedMeta.class);
  }

  @Override
  @Nullable
  public HaskellReservedVar getReservedVar() {
    return findChildByClass(HaskellReservedVar.class);
  }

}
