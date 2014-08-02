package com.haskforce.cabal.psi;

import com.haskforce.cabal.CabalLanguage;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class CabalTokenType extends IElementType {
    public CabalTokenType(@NotNull @NonNls String debugName) {
        super(debugName, CabalLanguage.INSTANCE);
    }

    @Override
    public String toString() {
        return "CabalTokenType." + super.toString();
    }
}
