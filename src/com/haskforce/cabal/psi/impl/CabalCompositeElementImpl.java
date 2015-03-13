package com.haskforce.cabal.psi.impl;

import com.haskforce.cabal.psi.CabalCompositeElement;
import com.haskforce.psi.HaskellCompositeElement;
import com.intellij.extapi.psi.ASTWrapperPsiElement;
import com.intellij.lang.ASTNode;

public class CabalCompositeElementImpl extends ASTWrapperPsiElement implements CabalCompositeElement {
    public CabalCompositeElementImpl(ASTNode node) {
        super(node);
    }

    @Override
    public String toString() {
        return getNode().getElementType().toString();
    }
}
