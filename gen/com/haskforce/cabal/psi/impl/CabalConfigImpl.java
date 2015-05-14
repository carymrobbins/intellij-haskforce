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

public class CabalConfigImpl extends CabalCompositeElementImpl implements CabalConfig {

  public CabalConfigImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof CabalVisitor) ((CabalVisitor)visitor).visitConfig(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public CabalBenchmark getBenchmark() {
    return findChildByClass(CabalBenchmark.class);
  }

  @Override
  @Nullable
  public CabalExecutable getExecutable() {
    return findChildByClass(CabalExecutable.class);
  }

  @Override
  @Nullable
  public CabalFlag getFlag() {
    return findChildByClass(CabalFlag.class);
  }

  @Override
  @Nullable
  public CabalLibrary getLibrary() {
    return findChildByClass(CabalLibrary.class);
  }

  @Override
  @Nullable
  public CabalSourceRepository getSourceRepository() {
    return findChildByClass(CabalSourceRepository.class);
  }

  @Override
  @Nullable
  public CabalTestSuite getTestSuite() {
    return findChildByClass(CabalTestSuite.class);
  }

}
