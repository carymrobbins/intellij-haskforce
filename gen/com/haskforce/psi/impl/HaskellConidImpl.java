// This is a generated file. Not intended for manual editing.
package com.haskforce.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.haskforce.psi.HaskellTypes.*;
import com.haskforce.stubs.HaskellConidStub;
import com.haskforce.psi.*;
import com.intellij.navigation.ItemPresentation;
import com.intellij.psi.PsiReference;
import com.intellij.psi.stubs.IStubElementType;

public class HaskellConidImpl extends HaskellNamedStubbedPsiElementBase<HaskellConidStub> implements HaskellConid {

  public HaskellConidImpl(ASTNode node) {
    super(node);
  }

  public HaskellConidImpl(HaskellConidStub stub, IStubElementType nodeType) {
    super(stub, nodeType);
  }

  public void accept(@NotNull HaskellVisitor visitor) {
    visitor.visitConid(this);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) accept((HaskellVisitor)visitor);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public PsiElement getConidRegexp() {
    return notNullChild(findChildByType(CONIDREGEXP));
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
