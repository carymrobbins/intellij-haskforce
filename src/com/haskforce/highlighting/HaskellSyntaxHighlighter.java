package com.haskforce.highlighting;

import static com.haskforce.psi.HaskellTypes.*;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

import com.haskforce.psi.HaskellTypes;

import java.util.*;

public class HaskellSyntaxHighlighter extends SyntaxHighlighterBase {

    /**
     * Tokens of bracket symbols.
     */
    public static final HashSet<IElementType> BRACKET_TOKENS = new HashSet<>(
      Arrays.asList(LBRACKET, RBRACKET, LTHOPEN, RTHCLOSE, QQOPEN));

    /**
     * Parens tokens
     */
    public static final Set<IElementType> PARENS_TOKENS = new HashSet<>(
      Arrays.asList(LPAREN, RPAREN, PARENSPLICE)
    );

    /**
     * Brace tokens
     */
    public static final Set<IElementType> BRACE_TOKENS = new HashSet<>(
      Arrays.asList(LBRACE, RBRACE)
    );

    /**
     * Tokens of reserved IDs.
     */
    public static final HashSet<IElementType> RESERVED_IDS_TOKENS = new HashSet<IElementType>(
      Arrays.asList(AS, CASE, CLASSTOKEN, DATA, DEFAULT
        , DERIVING, DO, ELSE, FORALLTOKEN, FOREIGN, HIDING, IF, IMPORT, IN, INFIX
        , INFIXL, INFIXR, HaskellTypes.INSTANCE, LET, MDOTOK, MODULETOKEN
        , NEWTYPE, OF, QUALIFIED, RECTOK, THEN, TYPE, WHERE));

    public static final HashSet<IElementType> RESERVED_OPS_TOKENS = new HashSet<IElementType>(
      Arrays.asList(DOUBLEPERIOD, COLON, DOUBLECOLON, EQUALS, BACKSLASH, PIPE, LEFTARROW, RIGHTARROW
        , AMPERSAT, TILDE, DOUBLEARROW));

    /*
     * Constructors, type classes, data types, .., are nice to distinguish
     * from the keywords. Borrow the color of INSTANCE_FIELD since it stands
     * out from the keywords by default and we are unlikely need it for other
     * purposes.
     */

    /*
     * We classify the reserved IDs as keywords.
     */
    public static final TextAttributesKey RESERVED_ID
            = TextAttributesKey.createTextAttributesKey("HS_RESERVED_ID", DefaultLanguageHighlighterColors.KEYWORD);
    public static final TextAttributesKey RESERVED_OP
            = TextAttributesKey.createTextAttributesKey("HS_RESERVED_OP", DefaultLanguageHighlighterColors.PREDEFINED_SYMBOL);
    public static final TextAttributesKey COMMA
            = TextAttributesKey.createTextAttributesKey("HS_COMMA", DefaultLanguageHighlighterColors.COMMA);
    public static final TextAttributesKey SEMICOLON
            = TextAttributesKey.createTextAttributesKey("HS_SEMICOLON", DefaultLanguageHighlighterColors.SEMICOLON);
    public static final TextAttributesKey BRACKETS
            = TextAttributesKey.createTextAttributesKey("HS_BRACKETS", DefaultLanguageHighlighterColors.BRACKETS);
    public static final TextAttributesKey PARENTHESES
            = TextAttributesKey.createTextAttributesKey("HS_PARENTHESES", DefaultLanguageHighlighterColors.PARENTHESES);
    public static final TextAttributesKey BRACES
            = TextAttributesKey.createTextAttributesKey("HS_BRACES", DefaultLanguageHighlighterColors.BRACES);
    public static final TextAttributesKey NESTED_COMMENT
            = TextAttributesKey.createTextAttributesKey("HS_NESTED_COMMENT", DefaultLanguageHighlighterColors.BLOCK_COMMENT);
    public static final TextAttributesKey HADDOCK
            = TextAttributesKey.createTextAttributesKey("HS_HADDOCK", DefaultLanguageHighlighterColors.DOC_COMMENT);
    public static final TextAttributesKey COMMENT
            = TextAttributesKey.createTextAttributesKey("HS_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT);
    public static final TextAttributesKey INTEGER
            = TextAttributesKey.createTextAttributesKey("HS_INTEGER", DefaultLanguageHighlighterColors.NUMBER);
    public static final TextAttributesKey FLOAT
            = TextAttributesKey.createTextAttributesKey("HS_FLOAT", DefaultLanguageHighlighterColors.NUMBER);
    public static final TextAttributesKey CHAR
            = TextAttributesKey.createTextAttributesKey("HS_CHAR", DefaultLanguageHighlighterColors.NUMBER);
    public static final TextAttributesKey CONID //Constructors, type constructors and type classes
            = TextAttributesKey.createTextAttributesKey("HS_CONSTRUCTOR", DefaultLanguageHighlighterColors.INSTANCE_FIELD);
    public static final TextAttributesKey VARID
            = TextAttributesKey.createTextAttributesKey("HS_VARIABLE", DefaultLanguageHighlighterColors.FUNCTION_CALL);
    public static final TextAttributesKey PARAMETER
            = TextAttributesKey.createTextAttributesKey("HS_PARAMETER", DefaultLanguageHighlighterColors.PARAMETER);
    public static final TextAttributesKey INFIXVARID
            = TextAttributesKey.createTextAttributesKey("HS_INFIX_VARID", DefaultLanguageHighlighterColors.FUNCTION_CALL);
    public static final TextAttributesKey VARSYM
            = TextAttributesKey.createTextAttributesKey("HS_VARSYM", DefaultLanguageHighlighterColors.OPERATION_SIGN);
    public static final TextAttributesKey CONSYM
            = TextAttributesKey.createTextAttributesKey("HS_CONSYM", DefaultLanguageHighlighterColors.PREDEFINED_SYMBOL);
    public static final TextAttributesKey PRAGMA
            = TextAttributesKey.createTextAttributesKey("HS_PRAGMA", DefaultLanguageHighlighterColors.METADATA);
    public static final TextAttributesKey STRING
            = TextAttributesKey.createTextAttributesKey("HS_STRING", DefaultLanguageHighlighterColors.STRING);
    public static final TextAttributesKey QUASIQUOTE
            = TextAttributesKey.createTextAttributesKey("HS_QUASIQUOTE", DefaultLanguageHighlighterColors.STRING);
    public static final TextAttributesKey ESCAPE
            = TextAttributesKey.createTextAttributesKey("HS_ESCAPE", DefaultLanguageHighlighterColors.VALID_STRING_ESCAPE);
    public static final TextAttributesKey SIGNATURE =
            TextAttributesKey.createTextAttributesKey("HS_SIGNATURE");

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
        keys = new HashMap<>(0);
        keysPutEach(RESERVED_IDS_TOKENS, RESERVED_ID);
        keysPutEach(RESERVED_OPS_TOKENS, RESERVED_OP);
        keysPutEach(BRACKET_TOKENS, BRACKETS);
        keysPutEach(PARENS_TOKENS, PARENTHESES);
        keysPutEach(BRACE_TOKENS, BRACES);
        keysPutEach(Arrays.asList(HaskellTypes.DOUBLEQUOTE, HaskellTypes.STRINGTOKEN), STRING);
        keysPutEach(Arrays.asList(HaskellTypes.COMMENTTEXT, HaskellTypes.OPENCOM, HaskellTypes.CLOSECOM), NESTED_COMMENT);
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
        keys.put(HaskellTypes.QQTEXT, QUASIQUOTE);
        keys.put(HaskellTypes.INFIXVARID, INFIXVARID);
        keys.put(HaskellTypes.SHEBANGSTART, COMMENT);
        keys.put(HaskellTypes.SHEBANGPATH, COMMENT);
    }

    @NotNull
    @Override
    public Lexer getHighlightingLexer() {
        return new HaskellSyntaxHighlightingLexer();
    }

    @NotNull
    @Override
    public TextAttributesKey[] getTokenHighlights(IElementType tokenType) {
        return pack(keys.get(tokenType), EMPTY);
    }

}
