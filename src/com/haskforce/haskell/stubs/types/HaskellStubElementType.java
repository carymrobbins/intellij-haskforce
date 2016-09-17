package com.haskforce.haskell.stubs.types;

import com.haskforce.haskell.HaskellLanguage;
import com.haskforce.haskell.psi.HaskellCompositeElement;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.StubElement;
import org.jetbrains.annotations.NotNull;

public abstract class HaskellStubElementType<S extends StubElement<T>, T extends HaskellCompositeElement> extends IStubElementType<S, T> {
    public HaskellStubElementType(String debugName) {
        super(debugName, HaskellLanguage.INSTANCE);
    }

    @NotNull
    @Override
    public String getExternalId() {
        return "haskell." + super.toString();
    }
}
