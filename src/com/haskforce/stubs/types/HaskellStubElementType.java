package com.haskforce.stubs.types;

import com.haskforce.HaskellLanguage;
import com.haskforce.psi.HaskellCompositeElement;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.StubElement;
import org.jetbrains.annotations.NotNull;

public abstract class HaskellStubElementType<S extends StubElement<T>, T extends HaskellCompositeElement> extends IStubElementType<S, T> {
    public HaskellStubElementType(String debugName) {
        super(debugName, HaskellLanguage.INSTANCE);
    }

    @Override
    public void indexStub(@NotNull S stub, @NotNull IndexSink sink) {

    }

    @NotNull
    @Override
    public String getExternalId() {
        return "haskell." + super.toString();
    }
}
