package com.haskforce.tools.yesod.shakespeare.hamlet.psi;

import com.haskforce.tools.yesod.shakespeare.hamlet.HamletLanguage;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class HamletElementType extends IElementType {
    public HamletElementType(@NotNull @NonNls String debugName) {
        super(debugName, HamletLanguage.INSTANCE);
    }
}
