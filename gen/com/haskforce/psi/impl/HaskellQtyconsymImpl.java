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

public class HaskellQtyconsymImpl extends HaskellCompositeElementImpl implements HaskellQtyconsym {

  public HaskellQtyconsymImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitQtyconsym(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellQconsym getQconsym() {
    return findChildByClass(HaskellQconsym.class);
  }

  @Override
  @Nullable
  public HaskellQvarsym getQvarsym() {
    return findChildByClass(HaskellQvarsym.class);
  }

  @Override
  @Nullable
  public HaskellTyconsym getTyconsym() {
    return findChildByClass(HaskellTyconsym.class);
  }

}
