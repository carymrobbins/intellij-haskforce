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

public class CabalCondtionalKeyImpl extends CabalCompositeElementImpl implements CabalCondtionalKey {

  public CabalCondtionalKeyImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof CabalVisitor) ((CabalVisitor)visitor).visitCondtionalKey(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public CabalBuildInformation getBuildInformation() {
    return findChildByClass(CabalBuildInformation.class);
  }

  @Override
  @Nullable
  public CabalExecutableSpecificKeys getExecutableSpecificKeys() {
    return findChildByClass(CabalExecutableSpecificKeys.class);
  }

  @Override
  @Nullable
  public CabalKey getKey() {
    return findChildByClass(CabalKey.class);
  }

  @Override
  @Nullable
  public CabalLibrarySpecificKeys getLibrarySpecificKeys() {
    return findChildByClass(CabalLibrarySpecificKeys.class);
  }

}
