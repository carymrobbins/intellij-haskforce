package com.haskforce.psi;

import com.haskforce.HaskellIcons;
import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

import com.haskforce.HaskellFileType;
import com.haskforce.HaskellLanguage;

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
}
