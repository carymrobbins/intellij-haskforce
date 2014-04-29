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

public class HaskellApatImpl extends ASTWrapperPsiElement implements HaskellApat {

  public HaskellApatImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitApat(this);
    else super.accept(visitor);
  }

  @Override
  @Nullable
  public HaskellApat getApat() {
    return findChildByClass(HaskellApat.class);
  }

  @Override
  @Nullable
  public HaskellGcon getGcon() {
    return findChildByClass(HaskellGcon.class);
  }

  @Override
  @NotNull
  public List<HaskellPat> getPatList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPat.class);
  }

  @Override
  @Nullable
  public HaskellPstringtoken getPstringtoken() {
    return findChildByClass(HaskellPstringtoken.class);
  }

  @Override
  @Nullable
  public HaskellQcon getQcon() {
    return findChildByClass(HaskellQcon.class);
  }

  @Override
  @NotNull
  public List<HaskellQvar> getQvarList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQvar.class);
  }

  @Override
  @Nullable
  public HaskellVarsym getVarsym() {
    return findChildByClass(HaskellVarsym.class);
  }

  @Override
  @Nullable
  public PsiElement getAmpersat() {
    return findChildByType(AMPERSAT);
  }

  @Override
  @Nullable
  public PsiElement getChartoken() {
    return findChildByType(CHARTOKEN);
  }

  @Override
  @Nullable
  public PsiElement getFloattoken() {
    return findChildByType(FLOATTOKEN);
  }

  @Override
  @Nullable
  public PsiElement getIntegertoken() {
    return findChildByType(INTEGERTOKEN);
  }

  @Override
  @Nullable
  public PsiElement getLbrace() {
    return findChildByType(LBRACE);
  }

  @Override
  @Nullable
  public PsiElement getLbracket() {
    return findChildByType(LBRACKET);
  }

  @Override
  @Nullable
  public PsiElement getLparen() {
    return findChildByType(LPAREN);
  }

  @Override
  @Nullable
  public PsiElement getRbrace() {
    return findChildByType(RBRACE);
  }

  @Override
  @Nullable
  public PsiElement getRbracket() {
    return findChildByType(RBRACKET);
  }

  @Override
  @Nullable
  public PsiElement getRparen() {
    return findChildByType(RPAREN);
  }

  @Override
  @Nullable
  public PsiElement getTilde() {
    return findChildByType(TILDE);
  }

  @Override
  @Nullable
  public PsiElement getVaridRegexp() {
    return findChildByType(VARIDREGEXP);
  }

}
