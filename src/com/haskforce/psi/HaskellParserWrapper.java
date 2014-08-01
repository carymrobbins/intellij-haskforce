package com.haskforce.psi;

import com.haskforce.HaskellLanguage;
import com.haskforce.parser.HaskellParser;
import com.haskforce.parsing.HaskellParser2;
import com.haskforce.parsing._HaskellParsingLexer;
import com.intellij.lang.ASTNode;
import com.intellij.lang.ITokenTypeRemapper;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.impl.PsiBuilderImpl;
import com.intellij.lexer.FlexAdapter;
import com.intellij.openapi.util.Pair;
import com.intellij.psi.TokenType;
import com.intellij.psi.tree.IElementType;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;

/**
 * Wraps the entry-point for the Grammar-Kit parser to register
 * a token-remapper.
 */
public class HaskellParserWrapper extends HaskellParser {
    private static final Pair<Integer, Integer> INIT_KEY = Pair.create(0, 0);
    public int rbraceDebt;
    public int maxRbraceDebt;
    public int lastCountedTok;
    public boolean regressed;
    public _HaskellParsingLexer lexer;

    public final HashMap<Integer, Pair<Integer,Integer>> debtPoints = ContainerUtil.newHashMap();

    final ITokenTypeRemapper myRemapper = new ITokenTypeRemapper() {
        /**
         * Intercept synthetic rbraces and varsymplus tokens and correct them.
         */
        @Override
        public IElementType filter(IElementType source, int start, int end, CharSequence text) {
            int lastLastCountedTok = lastCountedTok;
            if (start > lastCountedTok) {
                lastCountedTok = start;
                regressed = false;
            } else {
                regressed = start != lastCountedTok;
            }
            if (HaskellTypes.WHITESPACERBRACETOK.equals(source)) {
                if (!regressed && rbraceDebt > 0) {
                    rbraceDebt--;
                    return TokenType.WHITE_SPACE;
                }
                return source;
            }
            if (!HaskellTypes.VARSYMTOKPLUS.equals(source)) return source;

            String token = text.toString();
            if (HaskellLanguage.RESERVEDOPS.contains(token)) {
                // Lexer somehow missed lexing the op if we end up here.
                throw new HaskellParser2.ParserErrorException("Internal Error: Unexpected reservedop: " + token);
            }

            if ("--".equals(token) || "---".equals(token)) {
                return HaskellTypes.DASHES;
            }

            return HaskellTypes.VARSYMTOK;
        }
    };

    @NotNull
    @Override
    public ASTNode parse(IElementType root_, PsiBuilder builder_) {
        maxRbraceDebt = -1;
        lastCountedTok = 0;
        regressed = false;
        debtPoints.clear();
        builder_.setTokenTypeRemapper(myRemapper);
        lexer =  (_HaskellParsingLexer) ((FlexAdapter) ((PsiBuilderImpl) builder_).getLexer()).getFlex();
        ASTNode node = super.parse(root_, builder_);
        return node;
    }

    /**
     * Increases how many synthetic rbraces the remapper should consume.
     */
    public boolean increaseRbraceDebt(int offset) {
        if (regressed || maxRbraceDebt < 1) return false;

        Pair<Integer,Integer> oldValue = ContainerUtil.getOrCreate(debtPoints, offset, INIT_KEY);
        Pair<Integer,Integer> newValue;

        Integer snd = oldValue.getSecond();

        if (oldValue.getFirst() == 0) {
            newValue = Pair.create(maxRbraceDebt, maxRbraceDebt - 1);
        } else {
            newValue = Pair.create(oldValue.getFirst(), --snd);
        }
        rbraceDebt++;
        maxRbraceDebt--;
        debtPoints.put(offset, newValue);
        return true;
    }
}
