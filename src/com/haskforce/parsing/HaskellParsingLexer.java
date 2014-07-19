package com.haskforce.parsing;

import com.haskforce.highlighting._HaskellParsingLexer;
import com.intellij.lexer.FlexAdapter;

public class HaskellParsingLexer extends FlexAdapter {
    public HaskellParsingLexer() {
        super(new _HaskellParsingLexer());
    }
}
