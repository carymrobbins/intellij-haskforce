package com.haskforce.haskell.highlighting;

import com.intellij.lexer.FlexAdapter;

public class HaskellSyntaxHighlightingLexer extends FlexAdapter {
    public HaskellSyntaxHighlightingLexer() {
        super(new _HaskellSyntaxHighlightingLexer());
    }
}
