package com.haskforce;

import com.intellij.lexer.FlexAdapter;

import java.io.Reader;

public class HaskellLexerAdapter extends FlexAdapter {
    public HaskellLexerAdapter() {
        super(new HaskellLexer((Reader) null));
    }
}
