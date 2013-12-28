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

public class HaskellEscapeImpl extends ASTWrapperPsiElement implements HaskellEscape {

  public HaskellEscapeImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitEscape(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellDecimal getDecimal() {
    return findChildByClass(HaskellDecimal.class);
  }

  @Override
  @Nullable
  public HaskellHexadecimal getHexadecimal() {
    return findChildByClass(HaskellHexadecimal.class);
  }

  @Override
  @Nullable
  public HaskellOctal getOctal() {
    return findChildByClass(HaskellOctal.class);
  }

  @Override
  @Nullable
  public PsiElement getAscii() {
    return findChildByType(ASCII);
  }

  @Override
  @Nullable
  public PsiElement getCharesc() {
    return findChildByType(CHARESC);
  }

}
