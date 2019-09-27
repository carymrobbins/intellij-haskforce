package com.haskforce.psi.impl;

import com.haskforce.HaskellIcons;
import com.haskforce.psi.HaskellConid;
import com.haskforce.psi.HaskellFile;
import com.haskforce.psi.references.HaskellReference;
import com.haskforce.stubs.HaskellConidStub;
import com.intellij.lang.ASTNode;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.stubs.IStubElementType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

abstract class HaskellConidBaseImpl
  extends HaskellNamedStubbedPsiElementBase<HaskellConidStub>
  implements HaskellConid {

  HaskellConidBaseImpl(@NotNull HaskellConidStub stub, IStubElementType nodeType) {
    super(stub, nodeType);
  }

  HaskellConidBaseImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public String getName() {
    HaskellConidStub stub = getStub();
    if (stub != null) return StringUtil.notNullize(stub.getName());
    return this.getText();
  }

  @Override
  @Nullable
  public PsiElement getNameIdentifier() {
    return this.getNode().getPsi();
  }

  @Override
  @Nullable
  public PsiElement setName(@NotNull String newName) {
    PsiElement e = HaskellElementFactory.createConidFromText(getProject(), newName);
    if (e == null) return null;
    this.replace(e);
    return this;
  }

  @Override
  @NotNull
  public PsiReference getReference() {
    return new HaskellReference(this, TextRange.from(0, getName().length()));
  }


  @Override
  @NotNull
  public ItemPresentation getPresentation() {
    return new ItemPresentation() {
      @Override
      public String getPresentableText() {
        return HaskellConidBaseImpl.this.getName();
      }

      /**
       * This is needed to decipher between files when resolving multiple references.
       */
      @Override
      @Nullable
      public String getLocationString() {
        final PsiFile psiFile = HaskellConidBaseImpl.this.getContainingFile();
        return psiFile instanceof HaskellFile ? ((HaskellFile) psiFile).getModuleOrFileName() : null;
      }

      @Override
      public Icon getIcon(boolean unused) {
        return HaskellIcons.FILE;
      }
    };
  }
}
