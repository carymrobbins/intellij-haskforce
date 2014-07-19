package com.haskforce.parsing;

import com.intellij.lexer.FlexAdapter;

public class HaskellParsingLexer extends FlexAdapter {
    public HaskellParsingLexer() {
        super(new _HaskellParsingLexer());
    }
}
