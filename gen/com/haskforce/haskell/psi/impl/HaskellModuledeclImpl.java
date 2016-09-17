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

public class HaskellModuledeclImpl extends HaskellCompositeElementImpl implements HaskellModuledecl {

  public HaskellModuledeclImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitModuledecl(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellExports getExports() {
    return PsiTreeUtil.getChildOfType(this, HaskellExports.class);
  }

  @Override
  @Nullable
  public HaskellExportsempty getExportsempty() {
    return PsiTreeUtil.getChildOfType(this, HaskellExportsempty.class);
  }

  @Override
  @Nullable
  public HaskellPpragma getPpragma() {
    return PsiTreeUtil.getChildOfType(this, HaskellPpragma.class);
  }

  @Override
  @Nullable
  public HaskellQconid getQconid() {
    return PsiTreeUtil.getChildOfType(this, HaskellQconid.class);
  }

  @Override
  @NotNull
  public PsiElement getModuletoken() {
    return notNullChild(findChildByType(MODULETOKEN));
  }

  @Override
  @Nullable
  public PsiElement getWhere() {
    return findChildByType(WHERE);
  }

}
