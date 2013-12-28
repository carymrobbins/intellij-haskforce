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

public class HaskellGraphicImpl extends ASTWrapperPsiElement implements HaskellGraphic {

  public HaskellGraphicImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitGraphic(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellDigit getDigit() {
    return findChildByClass(HaskellDigit.class);
  }

  @Override
  @Nullable
  public HaskellLarge getLarge() {
    return findChildByClass(HaskellLarge.class);
  }

  @Override
  @Nullable
  public HaskellSmall getSmall() {
    return findChildByClass(HaskellSmall.class);
  }

  @Override
  @Nullable
  public HaskellSpecial getSpecial() {
    return findChildByClass(HaskellSpecial.class);
  }

  @Override
  @Nullable
  public HaskellSymbol getSymbol() {
    return findChildByClass(HaskellSymbol.class);
  }

}
