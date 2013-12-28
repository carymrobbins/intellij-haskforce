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

public class HaskellLexemeImpl extends ASTWrapperPsiElement implements HaskellLexeme {

  public HaskellLexemeImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitLexeme(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellLiteral getLiteral() {
    return findChildByClass(HaskellLiteral.class);
  }

  @Override
  @Nullable
  public HaskellQconid getQconid() {
    return findChildByClass(HaskellQconid.class);
  }

  @Override
  @Nullable
  public HaskellQconsym getQconsym() {
    return findChildByClass(HaskellQconsym.class);
  }

  @Override
  @Nullable
  public HaskellQvarid getQvarid() {
    return findChildByClass(HaskellQvarid.class);
  }

  @Override
  @Nullable
  public HaskellQvarsym getQvarsym() {
    return findChildByClass(HaskellQvarsym.class);
  }

  @Override
  @Nullable
  public HaskellReservedid getReservedid() {
    return findChildByClass(HaskellReservedid.class);
  }

  @Override
  @Nullable
  public HaskellReservedop getReservedop() {
    return findChildByClass(HaskellReservedop.class);
  }

  @Override
  @Nullable
  public HaskellSpecial getSpecial() {
    return findChildByClass(HaskellSpecial.class);
  }

}
