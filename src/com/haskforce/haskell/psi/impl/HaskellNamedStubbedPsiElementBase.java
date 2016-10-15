package com.haskforce.haskell.psi.impl;

import com.haskforce.haskell.psi.HaskellNamedElement;
import com.intellij.lang.ASTNode;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import org.jetbrains.annotations.NotNull;

public abstract class HaskellNamedStubbedPsiElementBase<T extends StubElement<?>> extends HaskellStubbedPsiElementBase<T> implements HaskellNamedElement {
    public HaskellNamedStubbedPsiElementBase(@NotNull T stub, IStubElementType nodeType) {
        super(stub, nodeType);
    }

    public HaskellNamedStubbedPsiElementBase(ASTNode node) {
        super(node);
    }
}
