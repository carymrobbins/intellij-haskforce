package com.haskforce.stubs;

import com.haskforce.psi.HaskellConid;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.NamedStubBase;
import com.intellij.psi.stubs.StubElement;
import com.intellij.util.io.StringRef;

public class HaskellConidStub extends NamedStubBase<HaskellConid> {
    public HaskellConidStub(StubElement parent, IStubElementType elementType, StringRef name) {
        super(parent, elementType, name);
    }

    public HaskellConidStub(StubElement parent, IStubElementType elementType, String name) {
        super(parent, elementType, name);
    }
}
