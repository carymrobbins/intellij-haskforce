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

public class HaskellExportsImpl extends ASTWrapperPsiElement implements HaskellExports {

  public HaskellExportsImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitExports(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellCpp> getCppList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellCpp.class);
  }

  @Override
  @Nullable
  public HaskellExport getExport() {
    return findChildByClass(HaskellExport.class);
  }

  @Override
  @NotNull
  public PsiElement getLparen() {
    return findNotNullChildByType(LPAREN);
  }

  @Override
  @Nullable
  public PsiElement getRparen() {
    return findChildByType(RPAREN);
  }

}
