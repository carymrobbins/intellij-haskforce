// This is a generated file. Not intended for manual editing.
package com.haskforce.haskell.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.haskforce.haskell.psi.HaskellTypes.*;
import com.haskforce.haskell.stubs.HaskellVaridStub;
import com.haskforce.haskell.psi.*;
import com.intellij.navigation.ItemPresentation;
import com.intellij.psi.PsiReference;
import com.intellij.psi.stubs.IStubElementType;

public class HaskellVaridImpl extends HaskellNamedStubbedPsiElementBase<HaskellVaridStub> implements HaskellVarid {

  public HaskellVaridImpl(HaskellVaridStub stub, IStubElementType nodeType) {
    super(stub, nodeType);
  }

  public HaskellVaridImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitVarid(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public PsiElement getAs() {
    return findChildByType(AS);
  }

  @Override
  @Nullable
  public PsiElement getQualified() {
    return findChildByType(QUALIFIED);
  }

  @Override
  @Nullable
  public PsiElement getRectok() {
    return findChildByType(RECTOK);
  }

  @Override
  @Nullable
  public PsiElement getVaridRegexp() {
    return findChildByType(VARIDREGEXP);
  }

  @NotNull
  public String getName() {
    return HaskellPsiImplUtil.getName(this);
  }

  @Nullable
  public PsiElement getNameIdentifier() {
    return HaskellPsiImplUtil.getNameIdentifier(this);
  }

  @NotNull
  public PsiReference getReference() {
    return HaskellPsiImplUtil.getReference(this);
  }

  @Nullable
  public PsiElement setName(String newName) {
    return HaskellPsiImplUtil.setName(this, newName);
  }

  @NotNull
  public ItemPresentation getPresentation() {
    return HaskellPsiImplUtil.getPresentation(this);
  }

}
