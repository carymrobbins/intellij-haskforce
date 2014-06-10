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

public class HaskellBodyImpl extends ASTWrapperPsiElement implements HaskellBody {

  public HaskellBodyImpl(ASTNode node) {
    super(node);
  }

  public void accept(@NotNull PsiElementVisitor visitor) {
    if (visitor instanceof HaskellVisitor) ((HaskellVisitor)visitor).visitBody(this);
    else super.accept(visitor);
  }

  @Override
  @NotNull
  public List<HaskellCpp> getCppList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellCpp.class);
  }

  @Override
  @NotNull
  public List<HaskellImpdecl> getImpdeclList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellImpdecl.class);
  }

  @Override
  @NotNull
  public List<HaskellNcomment> getNcommentList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellNcomment.class);
  }

  @Override
  @NotNull
  public List<HaskellPpragma> getPpragmaList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPpragma.class);
  }

  @Override
  @NotNull
  public List<HaskellPstringtoken> getPstringtokenList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellPstringtoken.class);
  }

  @Override
  @NotNull
  public List<HaskellQconid> getQconidList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQconid.class);
  }

  @Override
  @NotNull
  public List<HaskellQconsym> getQconsymList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQconsym.class);
  }

  @Override
  @NotNull
  public List<HaskellQinfixconid> getQinfixconidList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQinfixconid.class);
  }

  @Override
  @NotNull
  public List<HaskellQinfixvarid> getQinfixvaridList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQinfixvarid.class);
  }

  @Override
  @NotNull
  public List<HaskellQvarid> getQvaridList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQvarid.class);
  }

  @Override
  @NotNull
  public List<HaskellQvarsym> getQvarsymList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellQvarsym.class);
  }

  @Override
  @NotNull
  public List<HaskellReservedop> getReservedopList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellReservedop.class);
  }

  @Override
  @NotNull
  public List<HaskellSpecial> getSpecialList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellSpecial.class);
  }

  @Override
  @NotNull
  public List<HaskellSymbol> getSymbolList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellSymbol.class);
  }

  @Override
  @NotNull
  public List<HaskellWhitechar> getWhitecharList() {
    return PsiTreeUtil.getChildrenOfTypeAsList(this, HaskellWhitechar.class);
  }

}
