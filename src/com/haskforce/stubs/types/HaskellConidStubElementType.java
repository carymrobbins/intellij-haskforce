package com.haskforce.stubs.types;

import com.haskforce.psi.HaskellConid;
import com.haskforce.psi.impl.HaskellConidImpl;
import com.haskforce.stubs.HaskellConidStub;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class HaskellConidStubElementType extends HaskellNamedStubElementType<HaskellConidStub, HaskellConid> {
    public HaskellConidStubElementType(String debugName) {
        super(debugName);
    }

    @Override
    public HaskellConid createPsi(@NotNull HaskellConidStub stub) {
        return new HaskellConidImpl(stub, this);
    }

    @Override
    public HaskellConidStub createStub(@NotNull HaskellConid psi, StubElement parentStub) {
        return new HaskellConidStub(parentStub, this, psi.getName());
    }

    @Override
    public void serialize(@NotNull HaskellConidStub stub, @NotNull StubOutputStream dataStream) throws IOException {
        dataStream.writeName(stub.getName());
    }

    @NotNull
    @Override
    public HaskellConidStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
        return new HaskellConidStub(parentStub, this, dataStream.readName());
    }
}
