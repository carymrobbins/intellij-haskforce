package com.haskforce.psi;

import com.haskforce.HaskellLanguage;
import com.haskforce.parser.HaskellParser;
import com.haskforce.parsing._HaskellParsingLexer;
import com.intellij.lang.ASTNode;
import com.intellij.lang.ITokenTypeRemapper;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.impl.PsiBuilderAdapter;
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
            // int lastLastCountedTok = lastCountedTok;
            if (start > lastCountedTok) {
                lastCountedTok = start;
                regressed = false;
            } else {
                regressed = start != lastCountedTok;
            }
            if (HaskellTypes.WHITESPACERBRACETOK.equals(source)) {
                if (rbraceDebt > 0) {
                    rbraceDebt--;
                    return TokenType.WHITE_SPACE;
                }
                return source;
            }
            if (!HaskellTypes.VARSYMTOKPLUS.equals(source)) return source;

            String token = text.toString();
            if (HaskellLanguage.RESERVEDOPS.contains(token)) {
                // Lexer somehow missed lexing the op if we end up here.
                throw new RuntimeException("Internal Error: Unexpected reservedop: " + token);
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
        lastCountedTok = -1;
        regressed = false;
        debtPoints.clear();
        builder_.setTokenTypeRemapper(myRemapper);
        lexer =  (_HaskellParsingLexer) ((FlexAdapter) ((PsiBuilderImpl) builder_).getLexer()).getFlex();
        ASTNode node = super.parse(root_, builder_);
//        ASTNode node = super.parse(root_, new MyDebugPsiBuilderWrapper(builder_));
        return node;
    }

    static class MyDebugPsiBuilderWrapper extends PsiBuilderAdapter {
        public MyDebugPsiBuilderWrapper(@NotNull PsiBuilder delegate) {
            super(delegate);
            delegate.setDebugMode(true);
        }

        @Override
        public void advanceLexer() {
            super.advanceLexer();
            System.out.println(
              "advanceLexer() : "
                + getCurrentOffset()
                + " | " + getTokenType()
                + "\n" + renderLine()
            );
        }

        private String renderLine() {
            int n = getCurrentOffset();
            CharSequence text = getOriginalText();
            int i = Math.max(0, Math.min(n, text.length() - 1));
            while (i > 0 && text.charAt(i) != '\n') --i;
            int start = i;
            i = n;
            while (i < text.length() - 1 && text.charAt(i) != '\n') ++i;
            int end = i;
            CharSequence s = text.subSequence(start, end);
            StringBuilder b = new StringBuilder(s.length() * 2 + 1);
            b.append(s);
            b.append('\n');
            for (int j = 1; j < n - start; ++j) b.append(' ');
            b.append('^');
            return b.toString();
        }

    }

    /**
     * Increases how many synthetic rbraces the remapper should consume.
     */
    public boolean increaseRbraceDebt(int offset) {
        if (maxRbraceDebt < 1) return false;

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
