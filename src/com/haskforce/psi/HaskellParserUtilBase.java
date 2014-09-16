package com.haskforce.psi;

import com.haskforce.HaskellParserDefinition;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.lang.parser.GeneratedParserUtilBase;
import com.intellij.openapi.util.Pair;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

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
        if (i == 0) return 0;

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

    public static boolean indented(@NotNull PsiBuilder builder, int level, boolean geq) {
        if (!(builder instanceof Builder)) return false;
        PsiParser wrapper = ((Builder) builder).parser;
        if (!(wrapper instanceof HaskellParserWrapper)) return false;
        if (builder.eof()) return false;

        IElementType currtok = builder.getTokenType();
        Pair<Integer, IElementType> prevtok = previousElem(builder);
        if (prevtok == null) return true;

        int offs = builder.getCurrentOffset();
        int line = StringUtil.offsetToLineNumber(builder.getOriginalText(), offs);
        int prevline = StringUtil.offsetToLineNumber(builder.getOriginalText(), offs + prevtok.getFirst());
        if (prevline == line) return true;

        int thisLineStart = StringUtil.lineColToOffset(builder.getOriginalText(), line, 0);
        int prevLineStart = StringUtil.lineColToOffset(builder.getOriginalText(), prevline, 0);
        CharSequence lineStuff = builder.getOriginalText().subSequence(thisLineStart, offs);
        CharSequence prevLineStuff = builder.getOriginalText().subSequence(prevLineStart, thisLineStart - 1);
        int indentation = indentationLevel(prevLineStuff);
        int myindentation = offs - thisLineStart;
        String tokName = builder.getTokenText();

        if (geq && myindentation >= indentation ||
                !geq && myindentation > indentation) return true;
        return false;
    }

    @Nullable
    public static Pair<Integer, IElementType> previousElem(@NotNull PsiBuilder builder) {
        int i = -1;
        IElementType t = builder.rawLookup(i);
        while (t != null &&
                (HaskellParserDefinition.COMMENTS.contains(t) ||
                    HaskellParserDefinition.WHITE_SPACES.contains(t))) {
            t = builder.rawLookup(--i);
        }
        return t == null ? null : Pair.create(i - 1, t);
    }

    public static int indentationLevel(CharSequence c) {
        int i = 0;
        while(i < c.length() && c.charAt(i) == ' ') {
            i++;
        }
        return i;
    }
}
