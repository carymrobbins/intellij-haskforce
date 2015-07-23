package com.haskforce.cabal.psi;

import com.intellij.lexer.FlexAdapter;

public class CabalParsingLexer extends FlexAdapter {
    public CabalParsingLexer() {
        super(new _CabalParsingLexer());
    }
}
