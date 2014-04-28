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

public class HaskellCdeclImpl extends ASTWrapperPsiElement implements HaskellCdecl {

  public HaskellCdeclImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitCdecl(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public HaskellCdecl getCdecl() {
    return findNotNullChildByClass(HaskellCdecl.class);
  }

  @Override
  @Nullable
  public HaskellFunlhs getFunlhs() {
    return findChildByClass(HaskellFunlhs.class);
  }

  @Override
  @Nullable
  public HaskellGendecl getGendecl() {
    return findChildByClass(HaskellGendecl.class);
  }

  @Override
  @Nullable
  public HaskellRhs getRhs() {
    return findChildByClass(HaskellRhs.class);
  }

  @Override
  @Nullable
  public HaskellVarsym getVarsym() {
    return findChildByClass(HaskellVarsym.class);
  }

  @Override
  @Nullable
  public PsiElement getLparen() {
    return findChildByType(LPAREN);
  }

  @Override
  @Nullable
  public PsiElement getRparen() {
    return findChildByType(RPAREN);
  }

  @Override
  @Nullable
  public PsiElement getVaridRegexp() {
    return findChildByType(VARIDREGEXP);
  }

}
