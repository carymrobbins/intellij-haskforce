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

public class HaskellQinfixvaridImpl extends ASTWrapperPsiElement implements HaskellQinfixvarid {

  public HaskellQinfixvaridImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitQinfixvarid(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellModulePrefix getModulePrefix() {
    return findChildByClass(HaskellModulePrefix.class);
  }

  @Override
  @NotNull
  public PsiElement getVaridRegexp() {
    return findNotNullChildByType(VARIDREGEXP);
  }

}
