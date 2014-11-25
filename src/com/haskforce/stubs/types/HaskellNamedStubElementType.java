package com.haskforce.stubs.types;

import com.haskforce.psi.HaskellNamedElement;
import com.haskforce.stubs.HaskellAllNameIndex;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.NamedStubBase;
import org.jetbrains.annotations.NotNull;

public abstract class HaskellNamedStubElementType<S extends NamedStubBase<T>, T extends HaskellNamedElement> extends HaskellStubElementType<S, T> {
    public HaskellNamedStubElementType(String debugName) {
        super(debugName);
    }

    @Override
    public void indexStub(@NotNull S stub, @NotNull IndexSink sink) {
        final String name = stub.getName();
        if (name != null) {
            sink.occurrence(HaskellAllNameIndex.KEY, name);
        }
    }
}
