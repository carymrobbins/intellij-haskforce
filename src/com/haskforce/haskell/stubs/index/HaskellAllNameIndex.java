package com.haskforce.haskell.stubs.index;

import com.haskforce.haskell.psi.HaskellNamedElement;
import com.intellij.psi.stubs.StringStubIndexExtension;
import com.intellij.psi.stubs.StubIndexKey;
import org.jetbrains.annotations.NotNull;

/**
 * Stub index to store all names defined in the project; specifically for the "go to symbol" feature.
 */
public class HaskellAllNameIndex extends StringStubIndexExtension<HaskellNamedElement> {
    public static final StubIndexKey<String, HaskellNamedElement> KEY = StubIndexKey.createIndexKey("haskell.all.name");
    public static final int VERSION = 0;

    @Override
    public int getVersion() {
        return super.getVersion() + VERSION;
    }

    @NotNull
    @Override
    public StubIndexKey<String, HaskellNamedElement> getKey() {
        return KEY;
    }
}
