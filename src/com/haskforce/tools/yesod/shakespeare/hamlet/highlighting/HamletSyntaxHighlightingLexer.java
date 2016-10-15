package com.haskforce.tools.yesod.shakespeare.hamlet.highlighting;

import com.haskforce.tools.yesod.shakespeare.hamlet.psi.HamletTypes;
import com.intellij.lexer.*;
import com.intellij.psi.tree.TokenSet;

public class HamletSyntaxHighlightingLexer extends MergingLexerAdapter {
    public static final TokenSet mergeMe = TokenSet.create(HamletTypes.HASKELL_CODE);

    public HamletSyntaxHighlightingLexer() {
        super(new FlexAdapter(new _HamletSyntaxHighlightingLexer()), mergeMe);
    }
}
