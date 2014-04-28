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

public class HaskellPatImpl extends ASTWrapperPsiElement implements HaskellPat {

  public HaskellPatImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitPat(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public HaskellLpat getLpat() {
    return findNotNullChildByClass(HaskellLpat.class);
  }

  @Override
  @Nullable
  public HaskellPat getPat() {
    return findChildByClass(HaskellPat.class);
  }

  @Override
  @Nullable
  public HaskellQconop getQconop() {
    return findChildByClass(HaskellQconop.class);
  }

}
