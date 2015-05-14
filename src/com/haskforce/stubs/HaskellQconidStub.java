package com.haskforce.stubs;

import com.haskforce.psi.HaskellQconid;
import com.intellij.psi.stubs.IStubElementType;
import com.intellij.psi.stubs.NamedStubBase;
import com.intellij.psi.stubs.StubElement;
import com.intellij.util.io.StringRef;

public class HaskellQconidStub extends NamedStubBase<HaskellQconid> {
    public HaskellQconidStub(StubElement parent, IStubElementType elementType, StringRef name) {
        super(parent, elementType, name);
    }

    public HaskellQconidStub(StubElement parent, IStubElementType elementType, String name) {
        super(parent, elementType, name);
    }
}
