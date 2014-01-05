package com.haskforce;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.colors.CodeInsightColors;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.io.Reader;

import com.haskforce.psi.HaskellTypes;

public class HaskellSyntaxHighlighter extends SyntaxHighlighterBase {
    public static final TextAttributesKey RESERVEDID = DefaultLanguageHighlighterColors.KEYWORD;
    public static final TextAttributesKey[] RESERVEDID_KEYS = new TextAttributesKey[]{RESERVEDID};

    public static final TextAttributesKey RESERVEDOP = DefaultLanguageHighlighterColors.KEYWORD;
    public static final TextAttributesKey[] RESERVEDOP_KEYS = new TextAttributesKey[]{RESERVEDOP};

    public static final TextAttributesKey MODULE = DefaultLanguageHighlighterColors.INTERFACE_NAME;
    public static final TextAttributesKey[] MODULE_KEYS = new TextAttributesKey[]{MODULE};

    public static final TextAttributesKey CONID = DefaultLanguageHighlighterColors.FUNCTION_DECLARATION;
    public static final TextAttributesKey[] CONID_KEYS = new TextAttributesKey[]{CONID};

    public static final TextAttributesKey VARID = DefaultLanguageHighlighterColors.IDENTIFIER;
    public static final TextAttributesKey[] VARID_KEYS = new TextAttributesKey[]{VARID};

    public static final TextAttributesKey VARSYM = DefaultLanguageHighlighterColors.OPERATION_SIGN;
    public static final TextAttributesKey[] VARSYM_KEYS = new TextAttributesKey[]{VARSYM};

    public static final TextAttributesKey CONSYM = DefaultLanguageHighlighterColors.OPERATION_SIGN;
    public static final TextAttributesKey[] CONSYM_KEYS = new TextAttributesKey[]{CONSYM};

    public static final TextAttributesKey SPECIAL = DefaultLanguageHighlighterColors.PARENTHESES;
    public static final TextAttributesKey[] SPECIAL_KEYS = new TextAttributesKey[]{SPECIAL};

    public static final TextAttributesKey STRING = DefaultLanguageHighlighterColors.STRING;
    public static final TextAttributesKey[] STRING_KEYS = new TextAttributesKey[]{STRING};

    public static final TextAttributesKey INTEGER = DefaultLanguageHighlighterColors.NUMBER;
    public static final TextAttributesKey[] INTEGER_KEYS = new TextAttributesKey[]{INTEGER};

    public static final TextAttributesKey FLOAT = DefaultLanguageHighlighterColors.NUMBER;
    public static final TextAttributesKey[] FLOAT_KEYS = new TextAttributesKey[]{FLOAT};

    public static final TextAttributesKey CHAR = DefaultLanguageHighlighterColors.NUMBER;
    public static final TextAttributesKey[] CHAR_KEYS = new TextAttributesKey[]{CHAR};

    public static final TextAttributesKey COMMENT = DefaultLanguageHighlighterColors.LINE_COMMENT;
    public static final TextAttributesKey[] COMMENT_KEYS = new TextAttributesKey[]{COMMENT};

    public static final TextAttributesKey NCOMMENT = DefaultLanguageHighlighterColors.BLOCK_COMMENT;
    public static final TextAttributesKey[] NCOMMENT_KEYS = new TextAttributesKey[]{NCOMMENT};

    public static final TextAttributesKey HADDOCK = DefaultLanguageHighlighterColors.DOC_COMMENT;
    public static final TextAttributesKey[] HADDOCK_KEYS = new TextAttributesKey[]{HADDOCK};

    public static final TextAttributesKey ESCAPE = DefaultLanguageHighlighterColors.VALID_STRING_ESCAPE;
    public static final TextAttributesKey[] ESCAPE_KEYS = new TextAttributesKey[]{ESCAPE};

    @NotNull
    @Override
    public Lexer getHighlightingLexer() {
        return new HaskellLexer();
    }

    @NotNull
    @Override
    public TextAttributesKey[] getTokenHighlights(IElementType tokenType) {
        if (tokenType.equals(HaskellTypes.CONID)) {
            return CONID_KEYS;
        } else if (tokenType.equals(HaskellTypes.COMMENT)) {
            return COMMENT_KEYS;
        } else if (tokenType.equals(HaskellTypes.HADDOCK)) {
            return HADDOCK_KEYS;
        }
        return EMPTY;
    }
}
