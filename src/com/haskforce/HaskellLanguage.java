package com.haskforce;

import com.haskforce.psi.HaskellTypes;
import com.intellij.lang.Language;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.containers.HashSet;

import java.util.Arrays;

import static com.haskforce.psi.HaskellTypes.*;

/**
 * Top level entry for IntelliJ. Also serves as a placeholder for various
 * language related things, for example various token categories.
 */
public class HaskellLanguage extends Language {
    public static final HaskellLanguage INSTANCE = new HaskellLanguage();

    private HaskellLanguage() {
        super("Haskell");
    }

    @Override
    public boolean isCaseSensitive() {
        return true;
    }

    /**
     * Strings and tokens of specials.
     */
    public static final HashSet<String> SPECIALS = new HashSet<String>(
            Arrays.asList(new String[]{"(", ")", ",", ";", "["
                    , "]", "`", "{", "}"}));
    public static final HashSet<IElementType> SPECIAL_TOKENS = new HashSet<IElementType>(
            Arrays.asList(new IElementType[]{LPAREN, RPAREN, COMMA, SEMICOLON
                    , LBRACKET, RBRACKET, LBRACE, RBRACE, PARENSPLICE, LTHOPEN, RTHCLOSE}));

    /**
     * Strings and tokens of symbols.
     */
    public static final HashSet<String> SYMBOLS = new HashSet<String>(
            Arrays.asList(new String[]{"!", "#", "$", "%", "&"
                    , "*", "+", ".", "/", "<", "="
                    , ">", "?", "@", "\\", "^", "|", "_"
                    , "~", ":"}));
    public static final HashSet<IElementType> SYMBOL_TOKENS = new HashSet<IElementType>(
            Arrays.asList(new IElementType[]{EXCLAMATION, HASH, DOLLAR, PERCENT, AMPERSAND
                    , ASTERISK, PLUS, PERIOD, SLASH, LESSTHAN, EQUALS
                    , GREATERTHAN, QUESTION, AMPERSAT, BACKSLASH, CARET, PIPE, UNDERSCORE
                    , TILDE, COLON}));

    /**
     * Tokens of reserved IDs.
     */
    public static final HashSet<IElementType> RESERVED_IDS_TOKENS = new HashSet<IElementType>(
            Arrays.asList(new IElementType[]{AS, CASE, CLASSTOKEN, DATA, DEFAULT
                    , DERIVING, DO, ELSE, FOREIGN, HIDING, IF, IMPORT, IN, INFIX
                    , INFIXL, INFIXR, HaskellTypes.INSTANCE, LET, MODULETOKEN
                    , NEWTYPE, OF, QUALIFIED, THEN, TYPE, WHERE}));

    /**
     * Strings of reserved ops.
     */
    public static final HashSet<String> RESERVEDOPS = new HashSet<String>(
            Arrays.asList(new String[]{"..", ":", "::", "=", "\\"
                    , "|", "<-", "->", "@", "~", "=>"}));
}
