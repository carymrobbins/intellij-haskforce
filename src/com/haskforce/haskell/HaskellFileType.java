package com.haskforce.haskell;

import com.intellij.openapi.fileTypes.LanguageFileType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class HaskellFileType extends LanguageFileType {
    public static final HaskellFileType INSTANCE = new HaskellFileType();

    private HaskellFileType() {
        super(HaskellLanguage.INSTANCE);
    }

    @NotNull
    @Override
    public String getName() {
        return "Haskell";
    }

    @NotNull
    @Override
    public String getDescription() {
        return "Haskell language file";
    }

    @NotNull
    @Override
    public String getDefaultExtension() {
        return "hs";
    }

    @Nullable
    @Override
    public Icon getIcon() {
        return HaskellIcons.FILE;
    }
}
