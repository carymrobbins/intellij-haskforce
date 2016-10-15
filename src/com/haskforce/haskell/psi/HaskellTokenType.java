package com.haskforce.haskell.psi;

import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import com.haskforce.haskell.HaskellLanguage;

public class HaskellTokenType extends IElementType {
    public HaskellTokenType(@NotNull @NonNls String debugName) {
        super(debugName, HaskellLanguage.INSTANCE);
    }

    @Override
    public String toString() {
        return "HaskellTokenType." + super.toString();
    }
}
