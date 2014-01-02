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

public class HaskellQvarsymImpl extends ASTWrapperPsiElement implements HaskellQvarsym {

  public HaskellQvarsymImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitQvarsym(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellModid getModid() {
    return findChildByClass(HaskellModid.class);
  }

  @Override
  @NotNull
  public HaskellVarsym getVarsym() {
    return findNotNullChildByClass(HaskellVarsym.class);
  }

}
