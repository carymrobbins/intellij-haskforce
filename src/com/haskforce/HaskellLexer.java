package com.haskforce;

import com.intellij.lexer.FlexAdapter;

/**
 * Created by crobbins on 12/28/13.
 */
public class HaskellLexer extends FlexAdapter {
    public HaskellLexer() {
        super(new _HaskellLexer());
    }
}
