// This is a generated file. Not intended for manual editing.
package com.haskforce.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.haskforce.psi.HaskellTypes.*;
import com.haskforce.stubs.HaskellVaridStub;
import com.haskforce.psi.*;
import com.intellij.navigation.ItemPresentation;
import com.intellij.psi.PsiReference;
import com.intellij.psi.stubs.IStubElementType;

public class HaskellVaridImpl extends HaskellNamedStubbedPsiElementBase<HaskellVaridStub> implements HaskellVarid {

  public HaskellVaridImpl(@NotNull HaskellVaridStub stub, IStubElementType type) {
    super(stub, type);
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

  @Override
  @NotNull
  public String getName() {
    return HaskellPsiImplUtil.getName(this);
  }

  @Override
  @Nullable
  public PsiElement getNameIdentifier() {
    return HaskellPsiImplUtil.getNameIdentifier(this);
  }

  @Override
  @NotNull
  public PsiReference getReference() {
    return HaskellPsiImplUtil.getReference(this);
  }

  @Override
  @Nullable
  public PsiElement setName(@NotNull String newName) {
    return HaskellPsiImplUtil.setName(this, newName);
  }

  @Override
  @NotNull
  public ItemPresentation getPresentation() {
    return HaskellPsiImplUtil.getPresentation(this);
  }

}
