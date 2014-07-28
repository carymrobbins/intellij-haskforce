package com.haskforce.psi;

import com.haskforce.HaskellLanguage;
import com.haskforce.parser.HaskellParser;
import com.haskforce.parsing.HaskellParser2;
import com.intellij.lang.ASTNode;
import com.intellij.lang.ITokenTypeRemapper;
import com.intellij.lang.PsiBuilder;
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
    private int rbraceDebt;

    private final HashMap<Integer, Boolean> debtPoints = ContainerUtil.newHashMap();

    final ITokenTypeRemapper myRemapper = new ITokenTypeRemapper() {
        /**
         * Intercept synthetic rbraces and varsymplus tokens and correct them.
         */
        @Override
        public IElementType filter(IElementType source, int start, int end, CharSequence text) {
            if (rbraceDebt > 0 && HaskellTypes.WHITESPACERBRACETOK.equals(source)) {
                rbraceDebt--;
                return TokenType.WHITE_SPACE;
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
        builder_.setTokenTypeRemapper(myRemapper);
        ASTNode node = super.parse(root_, builder_);
        debtPoints.clear();
        return node;
    }

    /**
     * Increases how many synthetic rbraces the remapper should consume.
     */
    public void increaseRbraceDebt(int offset) {
        if (debtPoints.containsKey(offset)) return;

        rbraceDebt++;
        debtPoints.put(offset, true);
    }
}
