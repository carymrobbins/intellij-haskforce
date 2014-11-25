package com.haskforce.stubs;

import com.haskforce.psi.HaskellVarid;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.NamedStubBase;
import com.intellij.psi.stubs.StubElement;
import com.intellij.util.io.StringRef;

public class HaskellVaridStub extends NamedStubBase<HaskellVarid> {
    public HaskellVaridStub(StubElement parent, IStubElementType elementType, StringRef name) {
        super(parent, elementType, name);
    }

    public HaskellVaridStub(StubElement parent, IStubElementType elementType, String name) {
        super(parent, elementType, name);
    }
}
