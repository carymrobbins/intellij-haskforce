package com.haskforce.yesod.shakespeare.hamlet.psi;

import com.haskforce.yesod.shakespeare.hamlet.HamletLanguage;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NonNls;
import org.jetbrains.annotations.NotNull;

public class HamletTokenType extends IElementType {
    public HamletTokenType(@NotNull @NonNls String debugName) {
        super(debugName, HamletLanguage.INSTANCE);
    }

    @Override
    public String toString() {
        return "HamletTokenType." + super.toString();
    }
}
