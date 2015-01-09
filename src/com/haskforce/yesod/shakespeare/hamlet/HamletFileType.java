package com.haskforce.yesod.shakespeare.hamlet;

import com.intellij.openapi.fileTypes.LanguageFileType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import javax.swing.*;

public class HamletFileType extends LanguageFileType {
    public static final HamletFileType INSTANCE = new HamletFileType();

    private HamletFileType() {
        super(HamletLanguage.INSTANCE);
    }

    @NotNull
    @Override
    public String getName() {
        return "Hamlet";
    }

    @NotNull
    @Override
    public String getDescription() {
        return "Hamlet template file";
    }

    @NotNull
    @Override
    public String getDefaultExtension() {
        return "hamlet";
    }

    @Nullable
    @Override
    public Icon getIcon() {
        return HamletIcons.FILE;
    }
}
