package com.haskforce.stubs;

import com.haskforce.psi.HaskellVarid;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.NamedStubBase;
import com.intellij.psi.stubs.StubElement;
import com.intellij.util.io.StringRef;

public class HaskellConidStub extends NamedStubBase<HaskellVarid> {
    protected HaskellConidStub(StubElement parent, IStubElementType elementType, StringRef name) {
        super(parent, elementType, name);
    }

    protected HaskellConidStub(StubElement parent, IStubElementType elementType, String name) {
        super(parent, elementType, name);
    }
}
