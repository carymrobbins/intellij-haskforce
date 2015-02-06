package com.haskforce.cabal.psi;

import com.intellij.lexer.FlexAdapter;
import com.intellij.lexer.FlexLexer;

/**
 * Created by kasper on 1/02/15.
 */
public class CabalLexer extends FlexAdapter {
    public CabalLexer() {
        super(new _CabalLexer());
    }
}
