package com.haskforce.highlighting;

import com.intellij.lexer.Lexer;
import com.intellij.openapi.editor.DefaultLanguageHighlighterColors;
import com.intellij.openapi.editor.colors.TextAttributesKey;
import com.intellij.openapi.editor.colors.CodeInsightColors;
import com.intellij.openapi.fileTypes.SyntaxHighlighterBase;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.containers.HashSet;
import org.jetbrains.annotations.NotNull;

import java.awt.*;
import java.io.Reader;
import java.util.*;

import com.haskforce.psi.HaskellTypes;

import static com.haskforce.psi.HaskellTypes.*;

public class HaskellSyntaxHighlighter extends SyntaxHighlighterBase {
    /*
     * Constructors, type classes, data types, .., are nice to distinguish
     * from the keywords. Borrow the color of INSTANCE_FIELD since it stands
     * out from the keywords by default and we are unlikely need it for other
     * purposes.
     */
    public static final TextAttributesKey RESERVEDID = TextAttributesKey.createTextAttributesKey(
            "HASKELL_RESERVEDID", DefaultLanguageHighlighterColors.KEYWORD);
    public static final TextAttributesKey[] RESERVEDID_KEYS = new TextAttributesKey[]{RESERVEDID};

    public static final TextAttributesKey RESERVEDOP = TextAttributesKey.createTextAttributesKey(
            "HASKELL_RESERVEDOP", DefaultLanguageHighlighterColors.KEYWORD);
    public static final TextAttributesKey[] RESERVEDOP_KEYS = new TextAttributesKey[]{RESERVEDOP};

    public static final TextAttributesKey MODULE = TextAttributesKey.createTextAttributesKey(
            "HASKELL_MODULE", DefaultLanguageHighlighterColors.INTERFACE_NAME);
    public static final TextAttributesKey[] MODULE_KEYS = new TextAttributesKey[]{MODULE};

    public static final TextAttributesKey CONID =  TextAttributesKey.createTextAttributesKey(
            "HASKELL_CONID", DefaultLanguageHighlighterColors.INSTANCE_FIELD);
    public static final TextAttributesKey[] CONID_KEYS = new TextAttributesKey[]{CONID};

    public static final TextAttributesKey VARID = TextAttributesKey.createTextAttributesKey(
            "HASKELL_VARID", DefaultLanguageHighlighterColors.IDENTIFIER);
    public static final TextAttributesKey[] VARID_KEYS = new TextAttributesKey[]{VARID};

    public static final TextAttributesKey INFIXVARID = TextAttributesKey.createTextAttributesKey(
            "HASKELL_INFIXVARID", DefaultLanguageHighlighterColors.METADATA);
    public static final TextAttributesKey[] INFIXVARID_KEYS = new TextAttributesKey[]{INFIXVARID};

    public static final TextAttributesKey VARSYM = TextAttributesKey.createTextAttributesKey(
            "HASKELL_VARSYM", DefaultLanguageHighlighterColors.OPERATION_SIGN);
    public static final TextAttributesKey[] VARSYM_KEYS = new TextAttributesKey[]{VARSYM};

    public static final TextAttributesKey CONSYM = TextAttributesKey.createTextAttributesKey(
            "HASKELL_CONSYM", DefaultLanguageHighlighterColors.OPERATION_SIGN);
    public static final TextAttributesKey[] CONSYM_KEYS = new TextAttributesKey[]{CONSYM};

    public static final TextAttributesKey SPECIAL = TextAttributesKey.createTextAttributesKey(
            "HASKELL_SPECIAL", DefaultLanguageHighlighterColors.INSTANCE_FIELD);
    public static final TextAttributesKey[] SPECIAL_KEYS = new TextAttributesKey[]{SPECIAL};

    public static final TextAttributesKey STRING = TextAttributesKey.createTextAttributesKey(
            "HASKELL_STRING", DefaultLanguageHighlighterColors.STRING);
    public static final TextAttributesKey[] STRING_KEYS = new TextAttributesKey[]{STRING};

    public static final TextAttributesKey INTEGER = TextAttributesKey.createTextAttributesKey(
            "HASKELL_INTEGER", DefaultLanguageHighlighterColors.NUMBER);
    public static final TextAttributesKey[] INTEGER_KEYS = new TextAttributesKey[]{INTEGER};

    public static final TextAttributesKey FLOAT = TextAttributesKey.createTextAttributesKey(
            "HASKELL_FLOAT", DefaultLanguageHighlighterColors.NUMBER);
    public static final TextAttributesKey[] FLOAT_KEYS = new TextAttributesKey[]{FLOAT};

    public static final TextAttributesKey CHAR = TextAttributesKey.createTextAttributesKey(
            "HASKELL_CHAR", DefaultLanguageHighlighterColors.NUMBER);
    public static final TextAttributesKey[] CHAR_KEYS = new TextAttributesKey[]{CHAR};

    public static final TextAttributesKey COMMENT = TextAttributesKey.createTextAttributesKey(
            "HASKELL_COMMENT", DefaultLanguageHighlighterColors.LINE_COMMENT);
    public static final TextAttributesKey[] COMMENT_KEYS = new TextAttributesKey[]{COMMENT};

    public static final TextAttributesKey NCOMMENT = TextAttributesKey.createTextAttributesKey(
            "HASKELL_NCOMMENT", DefaultLanguageHighlighterColors.BLOCK_COMMENT);
    public static final TextAttributesKey[] NCOMMENT_KEYS = new TextAttributesKey[]{NCOMMENT};

    public static final TextAttributesKey HADDOCK = TextAttributesKey.createTextAttributesKey(
            "HASKELL_HADDOCK", DefaultLanguageHighlighterColors.DOC_COMMENT);
    public static final TextAttributesKey[] HADDOCK_KEYS = new TextAttributesKey[]{HADDOCK};

    public static final TextAttributesKey PRAGMA = TextAttributesKey.createTextAttributesKey(
            "HASKELL_PRAGMA", DefaultLanguageHighlighterColors.METADATA);
    public static final TextAttributesKey[] PRAGMA_KEYS = new TextAttributesKey[]{PRAGMA};

    public static final TextAttributesKey ESCAPE = TextAttributesKey.createTextAttributesKey(
            "HASKELL_ESCAPE", DefaultLanguageHighlighterColors.VALID_STRING_ESCAPE);
    public static final TextAttributesKey[] ESCAPE_KEYS = new TextAttributesKey[]{ESCAPE};

    /**
     * Tokens of reserved IDs.
     *
     * Used for mapping how the token should be highlighted.
     */
    private static final HashSet<IElementType> reservedIds = new HashSet<IElementType>(
            Arrays.asList(new IElementType[]{AS, CASE, CLASSTOKEN, DATA, DEFAULT
                    , DERIVING, DO, ELSE, FOREIGN, HIDING, IF, IMPORT, IN, INFIX
                    , INFIXL, INFIXR, INSTANCE, LET, HaskellTypes.MODULE
                    , NEWTYPE, OF, QUALIFIED, THEN, TYPE, WHERE}));

    @NotNull
    @Override
    public Lexer getHighlightingLexer() {
        return new HaskellSyntaxHighlightingLexer();
    }

    @NotNull
    @Override
    public TextAttributesKey[] getTokenHighlights(IElementType tokenType) {
        if (tokenType.equals(HaskellTypes.CONIDREGEXP)) {
            return CONID_KEYS;
        } else if (tokenType.equals(HaskellTypes.COMMENT)) {
            return COMMENT_KEYS;
        } else if (tokenType.equals(HaskellTypes.HADDOCK)) {
            return HADDOCK_KEYS;
        } else if (tokenType.equals(HaskellTypes.INTEGERTOKEN)) {
            return INTEGER_KEYS;
        } else if (tokenType.equals(HaskellTypes.FLOATTOKEN)) {
            return FLOAT_KEYS;
        } else if (tokenType.equals(HaskellTypes.CHARTOKEN)) {
            return CHAR_KEYS;
        } else if (tokenType.equals(HaskellTypes.STRINGTOKEN)) {
            return STRING_KEYS;
        } else if (tokenType.equals(HaskellTypes.SPECIAL)) {
            return SPECIAL_KEYS;
        } else if (tokenType.equals(HaskellTypes.NCOMMENT) ||
                   tokenType.equals(HaskellTypes.COMMENTTEXT)) {
            return NCOMMENT_KEYS;
        } else if (tokenType.equals(HaskellTypes.PRAGMA) ||
                   tokenType.equals(HaskellTypes.OPENPRAGMA) ||
                   tokenType.equals(HaskellTypes.CLOSEPRAGMA)) {
            return PRAGMA_KEYS;
        } else if (tokenType.equals(HaskellTypes.CONSYM)) {
            return CONSYM_KEYS;
        } else if (tokenType.equals(HaskellTypes.VARSYM)) {
            return VARSYM_KEYS;
        } else if (tokenType.equals(HaskellTypes.RESERVEDOP)) {
            return RESERVEDOP_KEYS;
        } else if (reservedIds.contains(tokenType)) {
            return RESERVEDID_KEYS;
        }
        return EMPTY;
    }
}
