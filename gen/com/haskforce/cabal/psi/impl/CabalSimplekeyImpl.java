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

public class CabalSimplekeyImpl extends CabalCompositeElementImpl implements CabalSimplekey {

  public CabalSimplekeyImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof CabalVisitor) ((CabalVisitor)visitor).visitSimplekey(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public CabalVarid getVarid() {
    return findChildByClass(CabalVarid.class);
  }

  @Override
  @Nullable
  public CabalVersion getVersion() {
    return findChildByClass(CabalVersion.class);
  }

}
