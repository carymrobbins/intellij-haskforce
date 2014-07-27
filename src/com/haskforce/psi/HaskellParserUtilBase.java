package com.haskforce.psi;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.lang.parser.GeneratedParserUtilBase;
import org.jetbrains.annotations.NotNull;

/**
 * Implementation holder for external rules in Haskell.bnf.
 */
public class HaskellParserUtilBase extends GeneratedParserUtilBase {
    /**
     * Called when the parser gets confused from layout rules.
     *
     * Increases the debt of rbraces in the token remapper in
     * HaskellParserWrapper. When the remapper is in debt it will swallow
     * (=remap them to TokenType.WHITE_SPACE) synthetic rbraces until no debt
     * remains.
     */
    public static boolean stateHackMess(@NotNull PsiBuilder builder,  int level) {
        if (!(builder instanceof Builder)) return false;
        PsiParser wrapper = ((Builder) builder).parser;
        if (!(wrapper instanceof HaskellParserWrapper)) return false;

        ((HaskellParserWrapper) wrapper).increaseRbraceDebt();
        return true;
/*
Debug utilities for future use.

        IElementType tok = builder.getTokenType();
        int offs = builder.getCurrentOffset();
        // System.out.println("Confused at: " + offs + " on token " + tok);
        return true;
*/
    }
}
