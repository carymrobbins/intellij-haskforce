package com.haskforce.stubs;

import com.haskforce.psi.HaskellFile;
import com.intellij.psi.stubs.PsiFileStubImpl;

/**
 * Basic implementation of a stub for a Haskell file so we can index its contents.
 */
public class HaskellFileStub extends PsiFileStubImpl<HaskellFile> {
    public HaskellFileStub(HaskellFile file) {
        super(file);
    }
}
