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

public class CabalTestSuiteSpecificKeysImpl extends CabalCompositeElementImpl implements CabalTestSuiteSpecificKeys {

  public CabalTestSuiteSpecificKeysImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof CabalVisitor) ((CabalVisitor)visitor).visitTestSuiteSpecificKeys(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public CabalMainIs getMainIs() {
    return findChildByClass(CabalMainIs.class);
  }

  @Override
  @Nullable
  public CabalTestModules getTestModules() {
    return findChildByClass(CabalTestModules.class);
  }

  @Override
  @Nullable
  public CabalTestSuiteType getTestSuiteType() {
    return findChildByClass(CabalTestSuiteType.class);
  }

}
