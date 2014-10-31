package com.haskforce.cabal.psi;

import com.haskforce.cabal.CabalFileType;
import com.haskforce.cabal.CabalIcons;
import com.haskforce.cabal.CabalLanguage;
import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class CabalFile extends PsiFileBase {
    public CabalFile(@NotNull FileViewProvider viewProvider) {
        super(viewProvider, CabalLanguage.INSTANCE);
    }

    @NotNull
    @Override
    public FileType getFileType() {
        return CabalFileType.INSTANCE;
    }

    @Override
    public String toString() {
        return "Cabal File";
    }

    @Nullable
    @Override
    public Icon getIcon(int flags) {
        return CabalIcons.FILE;
    }
}
