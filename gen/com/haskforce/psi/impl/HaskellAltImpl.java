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

public class HaskellAltImpl extends ASTWrapperPsiElement implements HaskellAlt {

  public HaskellAltImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitAlt(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellExp getExp() {
    return findChildByClass(HaskellExp.class);
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
  @NotNull
  public List<HaskellGuard> getGuardList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellGuard.class);
  }

  @Override
  @Nullable
  public HaskellImpdecl getImpdecl() {
    return findChildByClass(HaskellImpdecl.class);
  }

  @Override
  @NotNull
  public HaskellPat getPat() {
    return findNotNullChildByClass(HaskellPat.class);
  }

  @Override
  @NotNull
  public List<HaskellPpragma> getPpragmaList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPpragma.class);
  }

  @Override
  @Nullable
  public PsiElement getLbrace() {
    return findChildByType(LBRACE);
  }

  @Override
  @Nullable
  public PsiElement getPipe() {
    return findChildByType(PIPE);
  }

  @Override
  @Nullable
  public PsiElement getRbrace() {
    return findChildByType(RBRACE);
  }

  @Override
  @Nullable
  public PsiElement getRightarrow() {
    return findChildByType(RIGHTARROW);
  }

  @Override
  @Nullable
  public PsiElement getSemicolon() {
    return findChildByType(SEMICOLON);
  }

}
