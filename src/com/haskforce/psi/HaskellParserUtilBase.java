package com.haskforce.psi;

import com.intellij.lang.parser.GeneratedParserUtilBase;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.containers.HashSet;

import java.util.Arrays;

/**
 * Implementation holder for external rules in Haskell.bnf.
 */
public class HaskellParserUtilBase extends GeneratedParserUtilBase {
    public static final HashSet<String> HASKELL_SYMBOL = new HashSet<String>(
            Arrays.asList(new String[]{"!", "#", "$", "%", "&"
                    , "*", "+", ".", "/", "<", "="
                    , ">", "?", "@", "\\", "^", "|", "_"
                    , "~", ":"}));

    public static final HashSet<IElementType> HASKELL_SYMBOL_TOK = new HashSet<IElementType>(
            Arrays.asList(new IElementType[]{HaskellTypes.EXCLAMATION, HaskellTypes.HASH, HaskellTypes.DOLLAR, HaskellTypes.PERCENT, HaskellTypes.AMPERSAND
                    , HaskellTypes.ASTERISK, HaskellTypes.PLUS, HaskellTypes.PERIOD, HaskellTypes.SLASH, HaskellTypes.LESSTHAN, HaskellTypes.EQUALS
                    , HaskellTypes.GREATERTHAN, HaskellTypes.QUESTION, HaskellTypes.AMPERSAT, HaskellTypes.BACKSLASH, HaskellTypes.CARET, HaskellTypes.PIPE, HaskellTypes.UNDERSCORE
                    , HaskellTypes.TILDE, HaskellTypes.COLON}));

    public static final HashSet<String> HASKELL_SPECIAL = new HashSet<String>(
            Arrays.asList(new String[]{"(", ")", ",", ";", "["
                    , "]", "`", "{", "}"}));

    public static final HashSet<String> HASKELL_RESERVEDOP = new HashSet<String>(
            Arrays.asList(new String[]{"..", ":", "::", "=", "\\"
                    , "|", "<-", "->", "@", "~", "=>"}));
}
