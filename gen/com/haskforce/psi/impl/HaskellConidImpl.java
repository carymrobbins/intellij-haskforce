// This is a generated file. Not intended for manual editing.
package com.haskforce.psi.impl;

import java.util.List;
import org.jetbrains.annotations.*;
import com.intellij.lang.ASTNode;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementVisitor;
import com.intellij.psi.util.PsiTreeUtil;
import static com.haskforce.psi.HaskellTypes.*;
import com.haskforce.psi.*;
import com.haskforce.stubs.HaskellConidStub;
import com.intellij.psi.stubs.IStubElementType;

public class HaskellConidImpl extends HaskellConidBaseImpl implements HaskellConid {

  public HaskellConidImpl(@NotNull HaskellConidStub stub, IStubElementType type) {
    super(stub, type);
  }

  public HaskellConidImpl(ASTNode node) {
    super(node);
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

}
