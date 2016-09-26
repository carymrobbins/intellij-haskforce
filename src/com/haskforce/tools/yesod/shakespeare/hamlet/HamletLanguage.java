package com.haskforce.tools.yesod.shakespeare.hamlet;

import com.intellij.lang.Language;

public class HamletLanguage extends Language {
    public static final HamletLanguage INSTANCE = new HamletLanguage();

    private HamletLanguage() {
        super("Hamlet");
    }
}
