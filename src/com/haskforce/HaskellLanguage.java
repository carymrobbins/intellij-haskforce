package com.haskforce;

import com.intellij.lang.Language;

public class HaskellLanguage extends Language {
    public static final HaskellLanguage INSTANCE = new HaskellLanguage();

    private HaskellLanguage() {
        super("Haskell");
    }

    @Override
    public boolean isCaseSensitive() {
        return true;
    }
}
