package com.haskforce.psi.impl;

import com.haskforce.HaskellIcons;
import com.haskforce.psi.HaskellFile;
import com.haskforce.psi.HaskellVarid;
import com.haskforce.psi.references.HaskellReference;
import com.haskforce.stubs.HaskellVaridStub;
import com.intellij.lang.ASTNode;
import com.intellij.navigation.ItemPresentation;
import com.intellij.openapi.util.StackOverflowPreventedException;
import com.intellij.openapi.util.TextRange;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import com.intellij.psi.stubs.IStubElementType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

abstract class HaskellVaridBaseImpl
  extends HaskellNamedStubbedPsiElementBase<HaskellVaridStub>
  implements HaskellVarid {

  HaskellVaridBaseImpl(@NotNull HaskellVaridStub stub, IStubElementType nodeType) {
    super(stub, nodeType);
  }

  HaskellVaridBaseImpl(ASTNode node) {
    super(node);
  }

  @Override
  @NotNull
  public String getName() {
    HaskellVaridStub stub = getStub();
    if (stub != null) return StringUtil.notNullize(stub.getName());
    return getText();
  }

  @Override
  @NotNull
  public PsiReference getReference() {
    String s = getName();
    return new HaskellReference(this, TextRange.from(0, s.length()));
  }

  @Override
  @Nullable
  public PsiElement getNameIdentifier() {
    return getNode().getPsi();
  }

  @Override
  @Nullable
  public PsiElement setName(@NotNull String newName) {
    PsiElement e = HaskellElementFactory.createVaridFromText(getProject(), newName);
    if (e == null) return null;
    replace(e);
    return this;
  }

  @Override
  @NotNull
  public ItemPresentation getPresentation() {
    return new ItemPresentation() {
      @Override
      public String getPresentableText() {
        return HaskellVaridBaseImpl.this.getName();
      }

      /**
       * This is needed to decipher between files when resolving multiple references.
       */
      @Override
      @Nullable
      public String getLocationString() {
        try {
          final PsiFile psiFile = HaskellVaridBaseImpl.this.getContainingFile();
          return psiFile instanceof HaskellFile ? ((HaskellFile) psiFile).getModuleOrFileName() : null;
        } catch (StackOverflowPreventedException e) {
          // TODO: Figure out why this happens.
          throw e;
        }
      }

      @Override
      public Icon getIcon(boolean unused) {
        return HaskellIcons.FILE;
      }
    };
  }

}
