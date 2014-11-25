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

public class HaskellModuledeclImpl extends HaskellCompositeElementImpl implements HaskellModuledecl {

  public HaskellModuledeclImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitModuledecl(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellExports getExports() {
    return findChildByClass(HaskellExports.class);
  }

  @Override
  @Nullable
  public HaskellExportsempty getExportsempty() {
    return findChildByClass(HaskellExportsempty.class);
  }

  @Override
  @Nullable
  public HaskellPpragma getPpragma() {
    return findChildByClass(HaskellPpragma.class);
  }

  @Override
  @Nullable
  public HaskellQconid getQconid() {
    return findChildByClass(HaskellQconid.class);
  }

  @Override
  @NotNull
  public PsiElement getModuletoken() {
    return findNotNullChildByType(MODULETOKEN);
  }

  @Override
  @Nullable
  public PsiElement getWhere() {
    return findChildByType(WHERE);
  }

}
