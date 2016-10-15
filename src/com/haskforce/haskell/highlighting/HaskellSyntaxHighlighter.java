package com.haskforce.haskell.highlighting;

import com.haskforce.haskell.HaskellLanguage;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

import com.haskforce.haskell.psi.HaskellTypes;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

public class HaskellSyntaxHighlighter extends SyntaxHighlighterBase {
    /*
     * Constructors, type classes, data types, .., are nice to distinguish
     * from the keywords. Borrow the color of INSTANCE_FIELD since it stands
     * out from the keywords by default and we are unlikely need it for other
     * purposes.
     */

    // TODO: Create text attributes specifically for Haskell - https://confluence.jetbrains.com/pages/viewpage.action?pageId=49463468
    private static final TextAttributesKey JAVA_KEYWORD = TextAttributesKey.createTextAttributesKey("JAVA_KEYWORD", DefaultLanguageHighlighterColors.KEYWORD);

    public static final TextAttributesKey RESERVEDID = JAVA_KEYWORD;
    public static final TextAttributesKey RESERVEDOP = JAVA_KEYWORD;
    public static final TextAttributesKey COMMA = DefaultLanguageHighlighterColors.COMMA;
    public static final TextAttributesKey SEMICOLON = DefaultLanguageHighlighterColors.SEMICOLON;
    public static final TextAttributesKey BRACKETS = DefaultLanguageHighlighterColors.PARENTHESES;
    public static final TextAttributesKey NCOMMENT = DefaultLanguageHighlighterColors.BLOCK_COMMENT;
    public static final TextAttributesKey HADDOCK = DefaultLanguageHighlighterColors.DOC_COMMENT;
    public static final TextAttributesKey COMMENT = DefaultLanguageHighlighterColors.LINE_COMMENT;
    public static final TextAttributesKey INTEGER = DefaultLanguageHighlighterColors.NUMBER;
    public static final TextAttributesKey FLOAT = DefaultLanguageHighlighterColors.NUMBER;
    public static final TextAttributesKey CHAR = DefaultLanguageHighlighterColors.NUMBER;
    public static final TextAttributesKey CONID = DefaultLanguageHighlighterColors.INSTANCE_FIELD;
    public static final TextAttributesKey VARID = DefaultLanguageHighlighterColors.IDENTIFIER;
    public static final TextAttributesKey INFIXVARID = DefaultLanguageHighlighterColors.INSTANCE_FIELD;
    public static final TextAttributesKey VARSYM = DefaultLanguageHighlighterColors.IDENTIFIER;
    public static final TextAttributesKey CONSYM = DefaultLanguageHighlighterColors.INSTANCE_FIELD;
    public static final TextAttributesKey PRAGMA = DefaultLanguageHighlighterColors.METADATA;
    public static final TextAttributesKey STRING = DefaultLanguageHighlighterColors.STRING;
    public static final TextAttributesKey QQTEXT = DefaultLanguageHighlighterColors.STRING;
    public static final TextAttributesKey ESCAPE = DefaultLanguageHighlighterColors.VALID_STRING_ESCAPE;

    private static final Map<IElementType, TextAttributesKey> keys;

    /**
     * Helper to point multiple token types to a single color.
     */
    private static void keysPutEach(Iterable<IElementType> tokenTypes, TextAttributesKey value) {
        for (IElementType tokenType : tokenTypes) {
            keys.put(tokenType, value);
        }
    }

    static {
        keys = new HashMap<IElementType, TextAttributesKey>(0);
        keysPutEach(HaskellLanguage.RESERVED_IDS_TOKENS, RESERVEDID);
        keysPutEach(HaskellLanguage.RESERVED_OPS_TOKENS, RESERVEDOP);
        keysPutEach(HaskellLanguage.BRACKET_TOKENS, BRACKETS);
        keysPutEach(Arrays.asList(HaskellTypes.DOUBLEQUOTE, HaskellTypes.STRINGTOKEN), STRING);
        keysPutEach(Arrays.asList(HaskellTypes.COMMENTTEXT, HaskellTypes.OPENCOM, HaskellTypes.CLOSECOM), NCOMMENT);
        keysPutEach(Arrays.asList(HaskellTypes.PRAGMA, HaskellTypes.OPENPRAGMA, HaskellTypes.CLOSEPRAGMA), PRAGMA);
        keys.put(HaskellTypes.COMMA, COMMA);
        keys.put(HaskellTypes.SEMICOLON, SEMICOLON);
        keys.put(HaskellTypes.CONIDREGEXP, CONID);
        keys.put(HaskellTypes.COMMENT, COMMENT);
        keys.put(HaskellTypes.HADDOCK, HADDOCK);
        keys.put(HaskellTypes.INTEGERTOKEN, INTEGER);
        keys.put(HaskellTypes.FLOATTOKEN, FLOAT);
        keys.put(HaskellTypes.CHARTOKEN, CHAR);
        keys.put(HaskellTypes.VARSYMTOK, VARSYM);
        keys.put(HaskellTypes.CONSYMTOK, CONSYM);
        keys.put(HaskellTypes.QQTEXT, QQTEXT);
        keys.put(HaskellTypes.INFIXVARID, INFIXVARID);
        keys.put(HaskellTypes.SHEBANGSTART, COMMENT);
        keys.put(HaskellTypes.SHEBANGPATH, COMMENT);
    }

    @NotNull
    @Override
    public Lexer getHighlightingLexer() {
        return new com.haskforce.haskell.highlighting.HaskellSyntaxHighlightingLexer();
    }

    @NotNull
    @Override
    public TextAttributesKey[] getTokenHighlights(IElementType tokenType) {
        return pack(keys.get(tokenType), EMPTY);
    }
}
