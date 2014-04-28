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

public class HaskellExportImpl extends ASTWrapperPsiElement implements HaskellExport {

  public HaskellExportImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitExport(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellCnames getCnames() {
    return findChildByClass(HaskellCnames.class);
  }

  @Override
  @Nullable
  public HaskellCpp getCpp() {
    return findChildByClass(HaskellCpp.class);
  }

  @Override
  @Nullable
  public HaskellQconid getQconid() {
    return findChildByClass(HaskellQconid.class);
  }

  @Override
  @Nullable
  public HaskellQtycls getQtycls() {
    return findChildByClass(HaskellQtycls.class);
  }

  @Override
  @Nullable
  public HaskellQtycon getQtycon() {
    return findChildByClass(HaskellQtycon.class);
  }

  @Override
  @Nullable
  public HaskellQvar getQvar() {
    return findChildByClass(HaskellQvar.class);
  }

  @Override
  @Nullable
  public HaskellQvars getQvars() {
    return findChildByClass(HaskellQvars.class);
  }

}
