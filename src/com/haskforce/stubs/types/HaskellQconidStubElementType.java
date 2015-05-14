package com.haskforce.stubs.types;

import com.haskforce.psi.HaskellQconid;
import com.haskforce.psi.impl.HaskellQconidImpl;
import com.haskforce.stubs.HaskellQconidStub;
import com.haskforce.utils.HaskellUtil;
import com.intellij.lang.ASTNode;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

/**
 * Defines how and when to create stubs for conid elements for stub indexing.
 */
public class HaskellQconidStubElementType extends HaskellNamedStubElementType<HaskellQconidStub, HaskellQconid> {
    public HaskellQconidStubElementType(String debugName) {
        super(debugName);
    }

    @Override
    public HaskellQconid createPsi(@NotNull HaskellQconidStub stub) {
        return new HaskellQconidImpl(stub, this);
    }

    @Override
    public boolean shouldCreateStub(ASTNode node) {
        return HaskellUtil.definitionNode(node);
    }

    @Override
    public HaskellQconidStub createStub(@NotNull HaskellQconid psi, StubElement parentStub) {
        return new HaskellQconidStub(parentStub, this, psi.getName());
    }

    @Override
    public void serialize(@NotNull HaskellQconidStub stub, @NotNull StubOutputStream dataStream) throws IOException {
        dataStream.writeName(stub.getName());
    }

    @NotNull
    @Override
    public HaskellQconidStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
        return new HaskellQconidStub(parentStub, this, dataStream.readName());
    }
}
