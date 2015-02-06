package com.haskforce.cabal;

import com.intellij.lang.Language;

public class CabalLanguage extends Language {
    public static final CabalLanguage INSTANCE = new CabalLanguage();

    private CabalLanguage() {
        super("Cabal");
    }

    @Override
    public boolean isCaseSensitive() {
        return false;
    }
}
