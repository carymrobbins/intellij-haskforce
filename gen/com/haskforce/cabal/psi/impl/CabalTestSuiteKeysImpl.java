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

public class CabalTestSuiteKeysImpl extends CabalCompositeElementImpl implements CabalTestSuiteKeys {

  public CabalTestSuiteKeysImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof CabalVisitor) ((CabalVisitor)visitor).visitTestSuiteKeys(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public CabalBuildInformation getBuildInformation() {
    return findChildByClass(CabalBuildInformation.class);
  }

  @Override
  @Nullable
  public CabalConditional getConditional() {
    return findChildByClass(CabalConditional.class);
  }

  @Override
  @Nullable
  public CabalTestSuiteSpecificKeys getTestSuiteSpecificKeys() {
    return findChildByClass(CabalTestSuiteSpecificKeys.class);
  }

}
