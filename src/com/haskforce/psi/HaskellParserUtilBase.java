package com.haskforce.psi;

import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.lang.parser.GeneratedParserUtilBase;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

import java.util.LinkedList;

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

        IElementType tok = builder.getTokenType();
        int offs = builder.getCurrentOffset();
        int line = StringUtil.offsetToLineNumber(builder.getOriginalText(), offs);
        int lineStart = StringUtil.lineColToOffset(builder.getOriginalText(), line, 1);
        Pair<Integer, Integer> p = ((HaskellParserWrapper) wrapper).debtPoints.get(offs);
        if (p != null && p.getSecond() == 0 && !((HaskellParserWrapper) wrapper).regressed) {
            ((HaskellParserWrapper) wrapper).maxRbraceDebt = ((HaskellParserWrapper) wrapper).debtPoints.get(offs).getFirst();
            ((HaskellParserWrapper) wrapper).rbraceDebt = ((HaskellParserWrapper) wrapper).rbraceDebt - ((HaskellParserWrapper) wrapper).maxRbraceDebt;
            ((HaskellParserWrapper) wrapper).debtPoints.put(offs, Pair.create(((HaskellParserWrapper) wrapper).maxRbraceDebt, ((HaskellParserWrapper) wrapper).maxRbraceDebt));
        } else if (((HaskellParserWrapper) wrapper).maxRbraceDebt == -1) {
            int numOpen = findBraces(((HaskellParserWrapper) wrapper).lexer.openBraces, offs, line, lineStart);
            ((HaskellParserWrapper) wrapper).maxRbraceDebt = numOpen;
        }
        // System.out.println("Confused at: " + offs + " line " + line + " on token " + tok
        //        + " regressed: " + ((HaskellParserWrapper) wrapper).regressed + " max: "
        //        + ((HaskellParserWrapper) wrapper).maxRbraceDebt);

        boolean ret = ((HaskellParserWrapper) wrapper).increaseRbraceDebt(builder.getCurrentOffset());
        if (((HaskellParserWrapper) wrapper).maxRbraceDebt == 0) ((HaskellParserWrapper) wrapper).maxRbraceDebt = -1;
        return ret;
    }

    /**
     * Returns the number of stealable braces (=open braces - 1) in the parsing lexer at a
     * given line/column.
     */
    private static int findBraces(LinkedList<Pair<Pair<Integer, Integer>, Integer>> l, int offs, int line, int lineStart) {
        int i = l.size() - 1;
        Pair<Pair<Integer, Integer>, Integer> last;
        Pair<Pair<Integer, Integer>, Integer> e = last = l.get(i--);
        while (e != null) {
            if (e.getFirst().getFirst() > line ||
                    e.getFirst().getFirst() == line
                    && e.getFirst().getSecond() > offs - lineStart) {
                e = last;
                break;
            }
            last = e;
            e = l.get(i--);
        }
        // Comepnsate if we are looking for the last element.
        if (e == null) {
            e = last;
        }
        return e != null ? e.getSecond() - 1 : -1;
    }
}
