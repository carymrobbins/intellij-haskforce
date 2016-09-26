package com.haskforce.haskell.psi;

import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

import com.haskforce.haskell.HaskellLanguage;

public class HaskellElementType extends IElementType {
    public HaskellElementType(@NotNull @NonNls String debugName) {
        super(debugName, HaskellLanguage.INSTANCE);
    }
}
