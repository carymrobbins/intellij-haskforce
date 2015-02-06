package com.haskforce.cabal.psi.impl;

import com.haskforce.psi.HaskellCompositeElement;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;

public class CabalCompositeElementImpl extends ASTWrapperPsiElement implements HaskellCompositeElement {
    public CabalCompositeElementImpl(ASTNode node) {
        super(node);
    }

    @Override
    public String toString() {
        return getNode().getElementType().toString();
    }
}
