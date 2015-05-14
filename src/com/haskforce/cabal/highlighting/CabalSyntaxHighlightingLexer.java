package com.haskforce.cabal.highlighting;

import com.haskforce.cabal.psi._CabalParsingLexer;
import com.intellij.lexer.FlexAdapter;

public class CabalSyntaxHighlightingLexer extends FlexAdapter {
    public CabalSyntaxHighlightingLexer() {
        super(new _CabalParsingLexer());
    }
}
