package com.haskforce.psi;

import com.haskforce.HaskellIcons;
import com.haskforce.stubs.HaskellFileStub;
import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.stubs.StubElement;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

import com.haskforce.HaskellFileType;
import com.haskforce.HaskellLanguage;
import org.jetbrains.annotations.Nullable;

public class HaskellFile extends PsiFileBase {
    public HaskellFile(@NotNull FileViewProvider viewProvider) {
        super(viewProvider, HaskellLanguage.INSTANCE);
    }

    @NotNull
    @Override
    public FileType getFileType() {
        return HaskellFileType.INSTANCE;
    }

    @Override
    public String toString() {
        return "Haskell File";
    }

    @Override
    public Icon getIcon(int flags) {
        return HaskellIcons.FILE;
    }

    /**
     * Returns the module name defined in the file or null if it doesn't exist.
     */
    @Nullable
    public String getModuleName() {
        final HaskellModuledecl moduledecl = findChildByClass(HaskellModuledecl.class);
        if (moduledecl == null) { return null; }
        final HaskellQconid qconid = moduledecl.getQconid();
        if (qconid == null) { return null; }
        return qconid.getText();
    }

    /**
     * Returns the module name if it exists, otherwise returns the file name.
     */
    @NotNull
    public String getModuleOrFileName() {
        final String moduleName = getModuleName();
        return moduleName == null ? getName() : moduleName;
    }

    /**
     * Generates a stub for the current file, particularly so we can index names.
     */
    @Nullable
    @Override
    public HaskellFileStub getStub() {
        final StubElement stub = super.getStub();
        if (stub == null) return null;
        return (HaskellFileStub)stub;
    }
}
