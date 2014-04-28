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

public class HaskellLpatImpl extends ASTWrapperPsiElement implements HaskellLpat {

  public HaskellLpatImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitLpat(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellApat getApat() {
    return findChildByClass(HaskellApat.class);
  }

  @Override
  @Nullable
  public HaskellGcon getGcon() {
    return findChildByClass(HaskellGcon.class);
  }

  @Override
  @Nullable
  public PsiElement getFloattoken() {
    return findChildByType(FLOATTOKEN);
  }

  @Override
  @Nullable
  public PsiElement getIntegertoken() {
    return findChildByType(INTEGERTOKEN);
  }

  @Override
  @Nullable
  public PsiElement getMinus() {
    return findChildByType(MINUS);
  }

}
