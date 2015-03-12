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

public class CabalNumberImpl extends CabalCompositeElementImpl implements CabalNumber {

  public CabalNumberImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof CabalVisitor) ((CabalVisitor)visitor).visitNumber(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public PsiElement getNumberRegexp() {
    return findNotNullChildByType(NUMBERREGEXP);
  }

}
