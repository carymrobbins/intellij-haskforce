package com.haskforce.haskell.psi;

import com.haskforce.haskell.ui.HaskellIcons;
import com.haskforce.haskell.stubs.HaskellFileStub;
import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import com.intellij.psi.stubs.StubElement;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

import com.haskforce.haskell.HaskellFileType;
import com.haskforce.haskell.HaskellLanguage;
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
        final HaskellQconid qconid = getModuleElement();
        return qconid == null ? null : qconid.getText();
    }

    @Nullable
    public HaskellQconid getModuleElement() {
        final HaskellModuledecl moduledecl = findChildByClass(HaskellModuledecl.class);
        if (moduledecl == null) { return null; }
        return moduledecl.getQconid();
    }

    /**
     * Returns the module name if it exists, otherwise returns the file name.
     */
    @NotNull
    public String getModuleOrFileName() {
        final String moduleName = getModuleName();
        return moduleName == null ? getName() : moduleName;
    }

    @Nullable
    public HaskellBody getBody() {
        return PsiTreeUtil.getChildOfType(this, HaskellBody.class);
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
