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

public class HaskellQopImpl extends HaskellCompositeElementImpl implements HaskellQop {

  public HaskellQopImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitQop(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellQconop getQconop() {
    return findChildByClass(HaskellQconop.class);
  }

  @Override
  @Nullable
  public HaskellQvarop getQvarop() {
    return findChildByClass(HaskellQvarop.class);
  }

}
