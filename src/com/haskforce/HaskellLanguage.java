package com.haskforce;

import com.intellij.lang.Language;

/**
 * Top level entry for IntelliJ. Also serves as a placeholder for various
 * language related things, for example various token categories.
 */
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
