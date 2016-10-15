package com.haskforce.tools.yesod.shakespeare.hamlet.psi;

import com.haskforce.tools.yesod.shakespeare.hamlet.HamletFileType;
import com.haskforce.tools.yesod.shakespeare.hamlet.HamletIcons;
import com.haskforce.tools.yesod.shakespeare.hamlet.HamletLanguage;
import com.intellij.extapi.psi.PsiFileBase;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.FileViewProvider;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class HamletFile extends PsiFileBase {
    public HamletFile(@NotNull FileViewProvider viewProvider) {
        super(viewProvider, HamletLanguage.INSTANCE);
    }

    @NotNull
    @Override
    public FileType getFileType() {
        return HamletFileType.INSTANCE;
    }

    @Override
    public String toString() {
        return "Hamlet File";
    }

    @Nullable
    @Override
    public Icon getIcon(int flags) {
        return HamletIcons.FILE;
    }
}
