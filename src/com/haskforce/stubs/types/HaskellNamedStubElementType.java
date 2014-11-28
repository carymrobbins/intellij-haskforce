package com.haskforce.stubs.types;

import com.haskforce.psi.HaskellNamedElement;
import com.haskforce.stubs.index.HaskellAllNameIndex;
import com.haskforce.utils.HaskellUtil;
import com.intellij.psi.stubs.IndexSink;
import com.intellij.psi.stubs.NamedStubBase;
import org.jetbrains.annotations.NotNull;

/**
 * Define how to index stubs for named elements.
 */
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
