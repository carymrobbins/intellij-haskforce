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

public class HaskellNcommentImpl extends ASTWrapperPsiElement implements HaskellNcomment {

  public HaskellNcommentImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitNcomment(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellAny> getAnyList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellAny.class);
  }

  @Override
  @NotNull
  public List<HaskellNcomment> getNcommentList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellNcomment.class);
  }

  @Override
  @NotNull
  public List<HaskellSeq> getSeqList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellSeq.class);
  }

  @Override
  @NotNull
  public PsiElement getClosecom() {
    return findNotNullChildByType(CLOSECOM);
  }

  @Override
  @NotNull
  public PsiElement getOpencom() {
    return findNotNullChildByType(OPENCOM);
  }

}
