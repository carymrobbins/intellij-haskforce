// This is a generated file. Not intended for manual editing.
package com.haskforce.cabal.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.haskforce.cabal.psi.CabalTypes.*;
import com.haskforce.cabal.psi.*;
import com.intellij.psi.PsiReference;

public class CabalVaridImpl extends CabalCompositeElementImpl implements CabalVarid {

  public CabalVaridImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof CabalVisitor) ((CabalVisitor)visitor).visitVarid(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public PsiElement getVaridRegexp() {
    return findNotNullChildByType(VARIDREGEXP);
  }

  @NotNull
  public String getName() {
    return CabalPsiImplUtil.getName(this);
  }

  @NotNull
  public PsiReference getReference() {
    return CabalPsiImplUtil.getReference(this);
  }

  @Nullable
  public PsiElement getNameIdentifier() {
    return CabalPsiImplUtil.getNameIdentifier(this);
  }

  @Nullable
  public PsiElement setName(String newName) {
    return CabalPsiImplUtil.setName(this, newName);
  }

}
