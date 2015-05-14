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

public class CabalModuleImpl extends CabalCompositeElementImpl implements CabalModule {

  public CabalModuleImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof CabalVisitor) ((CabalVisitor)visitor).visitModule(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<CabalVarid> getVaridList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, CabalVarid.class);
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
