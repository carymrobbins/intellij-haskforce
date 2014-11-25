package com.haskforce.psi.impl;

import com.haskforce.stubs.types.HaskellConidStubElementType;
import com.haskforce.stubs.types.HaskellVaridStubElementType;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

public class HaskellElementTypeFactory {
    private HaskellElementTypeFactory() {}

    public static IElementType factory(@NotNull String name) {
        if (name.equals("CONID")) return new HaskellConidStubElementType(name);
        if (name.equals("VARID")) return new HaskellVaridStubElementType(name);
        throw new RuntimeException("Unknown element type: " + name);
    }
}
