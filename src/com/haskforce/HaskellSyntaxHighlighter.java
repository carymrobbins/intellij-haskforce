package com.haskforce;

import com.intellij.lexer.FlexAdapter;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.SyntaxHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.markup.TextAttributes;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import com.intellij.ui.JBColor;
import org.jetbrains.annotations.NotNull;

import static com.intellij.openapi.editor.colors.TextAttributesKey.createTextAttributesKey;

import java.awt.*;
import java.io.Reader;

import com.haskforce.psi.HaskellTypes;

public class HaskellSyntaxHighlighter extends SyntaxHighlighterBase {
    public static final TextAttributesKey SEPARATOR = createTextAttributesKey("HASKELL_SEPARATOR", SyntaxHighlighterColors.OPERATION_SIGN);
    public static final TextAttributesKey KEY = createTextAttributesKey("HASKELL_KEY", SyntaxHighlighterColors.KEYWORD);
    public static final TextAttributesKey VALUE = createTextAttributesKey("HASKELL_VALUE", SyntaxHighlighterColors.STRING);
    public static final TextAttributesKey COMMENT = createTextAttributesKey("HASKELL_COMMENT", SyntaxHighlighterColors.LINE_COMMENT);

    static final TextAttributesKey BAD_CHARACTER = createTextAttributesKey("HASKELL_BAD_CHARACTER",
            new TextAttributes(JBColor.RED, null, null, null, Font.BOLD));

    private static final TextAttributesKey[] BAD_CHAR_KEYS = new TextAttributesKey[]{BAD_CHARACTER};
    private static final TextAttributesKey[] SEPARATOR_KEYS = new TextAttributesKey[]{SEPARATOR};
    private static final TextAttributesKey[] KEY_KEYS = new TextAttributesKey[]{KEY};
    private static final TextAttributesKey[] VALUE_KEYS = new TextAttributesKey[]{VALUE};
    private static final TextAttributesKey[] COMMENT_KEYS = new TextAttributesKey[]{COMMENT};
    private static final TextAttributesKey[] EMPTY_KEYS = new TextAttributesKey[0];

    @NotNull
    @Override
    public Lexer getHighlightingLexer() {
        return new HaskellLexer();
    }

    @NotNull
    @Override
    public TextAttributesKey[] getTokenHighlights(IElementType tokenType) {
        return EMPTY_KEYS;
    }
}
