package com.haskforce.haskell.parsing;

import com.intellij.lexer.FlexAdapter;

public class HaskellParsingLexer extends FlexAdapter {
    public HaskellParsingLexer() {
        super(new com.haskforce.haskell.parsing._HaskellParsingLexer());
    }
}
