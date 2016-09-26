package com.haskforce.haskell.stubs.types;

import com.haskforce.haskell.psi.HaskellVarid;
import com.haskforce.haskell.psi.impl.HaskellVaridImpl;
import com.haskforce.haskell.stubs.HaskellVaridStub;
import com.haskforce.system.utils.HaskellUtil;
import com.intellij.lang.ASTNode;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

public class HaskellVaridStubElementType extends HaskellNamedStubElementType<HaskellVaridStub, HaskellVarid> {
    public HaskellVaridStubElementType(String debugName) {
        super(debugName);
    }

    @Override
    public HaskellVarid createPsi(@NotNull HaskellVaridStub stub) {
        return new HaskellVaridImpl(stub, this);
    }

    @Override
    public boolean shouldCreateStub(ASTNode node) {
        return HaskellUtil.definitionNode(node);
    }

    @Override
    public HaskellVaridStub createStub(@NotNull HaskellVarid psi, StubElement parentStub) {
        return new HaskellVaridStub(parentStub, this, psi.getName());
    }

    @Override
    public void serialize(@NotNull HaskellVaridStub stub, @NotNull StubOutputStream dataStream) throws IOException {
        dataStream.writeName(stub.getName());
    }

    @NotNull
    @Override
    public HaskellVaridStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
        return new HaskellVaridStub(parentStub, this, dataStream.readName());
    }
}
