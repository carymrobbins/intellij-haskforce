package com.haskforce.stubs.types;

import com.haskforce.HaskellLanguage;
import com.haskforce.psi.HaskellFile;
import com.haskforce.stubs.HaskellFileStub;
import com.intellij.psi.PsiFile;
import com.intellij.psi.StubBuilder;
import com.intellij.psi.stubs.DefaultStubBuilder;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.stubs.StubInputStream;
import com.intellij.psi.stubs.StubOutputStream;
import com.intellij.psi.tree.IStubFileElementType;
import org.jetbrains.annotations.NotNull;

import java.io.IOException;

/**
 * Define how and when to create a stub for a Haskell file so we can index its contents.
 */
public class HaskellFileStubElementType extends IStubFileElementType<HaskellFileStub> {
    public static final int VERSION = 0;
    public static final IStubFileElementType INSTANCE = new HaskellFileStubElementType();

    public HaskellFileStubElementType() {
        super("FILE", HaskellLanguage.INSTANCE);
    }

    @Override
    public StubBuilder getBuilder() {
        return new DefaultStubBuilder() {
            @NotNull
            @Override
            protected StubElement createStubForFile(@NotNull PsiFile file) {
                if (file instanceof HaskellFile) {
                    return new HaskellFileStub((HaskellFile)file);
                }
                return super.createStubForFile(file);
            }
        };
    }

    @Override
    public int getStubVersion() {
        return VERSION;
    }

    @Override
    public void serialize(@NotNull HaskellFileStub stub, @NotNull StubOutputStream dataStream) throws IOException {
        // TODO
    }

    @NotNull
    @Override
    public HaskellFileStub deserialize(@NotNull StubInputStream dataStream, StubElement parentStub) throws IOException {
        return new HaskellFileStub(null);
    }

    @NotNull
    @Override
    public String getExternalId() {
        return "haskell.FILE";
    }
}
