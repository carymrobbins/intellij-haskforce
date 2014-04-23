package com.haskforce.cabal;

import com.intellij.openapi.fileTypes.LanguageFileType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class CabalFileType extends LanguageFileType {
    public static final CabalFileType INSTANCE = new CabalFileType();

    private CabalFileType() {
        super(CabalLanguage.INSTANCE);
    }

    @NotNull
    @Override
    public String getName() {
        return "Cabal file";
    }

    @NotNull
    @Override
    public String getDescription() {
        return "Cabal build file";
    }

    @NotNull
    @Override
    public String getDefaultExtension() {
        return "cabal";
    }

    @Nullable
    @Override
    public Icon getIcon() {
        return CabalIcons.FILE;
    }
}
