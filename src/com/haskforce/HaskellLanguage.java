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
     * Tokens of bracket symbols.
     */
    public static final HashSet<IElementType> BRACKET_TOKENS = new HashSet<IElementType>(
            Arrays.asList(LPAREN, RPAREN, LBRACKET, RBRACKET, LBRACE, RBRACE, PARENSPLICE, LTHOPEN, RTHCLOSE, QQOPEN));

    /**
     * Tokens of reserved IDs.
     */
    public static final HashSet<IElementType> RESERVED_IDS_TOKENS = new HashSet<IElementType>(
            Arrays.asList(AS, CASE, CLASSTOKEN, DATA, DEFAULT
                    , DERIVING, DO, ELSE, FORALLTOKEN, FOREIGN, HIDING, IF, IMPORT, IN, INFIX
                    , INFIXL, INFIXR, HaskellTypes.INSTANCE, LET, MDOTOK, MODULETOKEN
                    , NEWTYPE, OF, QUALIFIED, RECTOK, THEN, TYPE, WHERE));

    /**
     * Strings of reserved ops.
     */
    public static final HashSet<String> RESERVEDOPS = new HashSet<String>(
            Arrays.asList("..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>"));

    public static final HashSet<IElementType> RESERVED_OPS_TOKENS = new HashSet<IElementType>(
            Arrays.asList(DOUBLEPERIOD, COLON, DOUBLECOLON, EQUALS, BACKSLASH, PIPE, LEFTARROW, RIGHTARROW
                    , AMPERSAT, TILDE, DOUBLEARROW));
}
