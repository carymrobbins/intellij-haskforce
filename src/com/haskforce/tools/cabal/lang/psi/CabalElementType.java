package com.haskforce.tools.cabal.lang.psi;

import com.haskforce.tools.cabal.CabalLanguage;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class CabalElementType extends IElementType {
    public CabalElementType(@NotNull @NonNls String debugName) {
        super(debugName, CabalLanguage.INSTANCE);
    }
}
