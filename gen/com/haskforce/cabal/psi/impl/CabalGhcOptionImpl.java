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

public class CabalGhcOptionImpl extends CabalCompositeElementImpl implements CabalGhcOption {

  public CabalGhcOptionImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof CabalVisitor) ((CabalVisitor)visitor).visitGhcOption(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<CabalNumber> getNumberList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, CabalNumber.class);
  }

  @Override
  @NotNull
  public List<CabalVarid> getVaridList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, CabalVarid.class);
  }

}
