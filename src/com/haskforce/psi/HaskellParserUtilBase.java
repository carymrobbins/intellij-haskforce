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

import java.util.Arrays;
import java.util.LinkedList;

import static com.intellij.openapi.util.text.StringUtil.parseInt;

/**
 * Implementation holder for external rules in Haskell.bnf.
 */
public class HaskellParserUtilBase extends GeneratedParserUtilBase {

    /**
     * This is mostly useful for debugging. Enabling this will allow us to
     * more easily inspect the stack trace at the position where some deep
     * recursion is happening, enabling us to then adjust the grammar and
     * see if we can
     */
    private static final boolean THROW_ON_MAX_RECURSION =
      "true".equals(System.getProperty("com.haskforce.parser.recursion.max.throw"));

    private static final int MAX_RECURSION_LEVEL =
      parseInt(System.getProperty("com.haskforce.parser.recursion.max"), 100);

    /**
     * HACK! This is pure copy-pasta from {@link com.intellij.lang.parser.GeneratedParserUtilBase}
     * We are abusing the static import of this class in {@link com.haskforce.parser.HaskellParser}
     * to "override" the {@link GeneratedParserUtilBase#recursion_guard_(com.intellij.lang.PsiBuilder, int, java.lang.String)}
     * static method.
     *
     * The problem is that, for large files, our parser may do a lot of backtracking, and this
     * helps us to prevent seemingly endless loops while parsing. Instead of using the default
     * max recursion level of 1000, we reduce this to 10.
     *
     * Note that changing this value WILL change the parse tree as it breaks
     * the red cuts in the parser.
     *
     * The real solution is to rewrite the parser, but this should provide some amount of
     * life support for the current parser.
     */

    public static boolean recursion_guard_(PsiBuilder builder, int level, String funcName) {
        if (level > MAX_RECURSION_LEVEL) {
            final String msg =
                "Maximum recursion level "
                    + "(" + MAX_RECURSION_LEVEL + ") "
                    + "reached in '" + funcName + "' "
                    + "at offset " + builder.getCurrentOffset();
            if (THROW_ON_MAX_RECURSION) throw new RuntimeException(msg);
            builder.mark().error(msg);
            return false;
        }
        return true;
    }

    public static boolean toplevel_recover_debug(PsiBuilder builder, int level) {
        IElementType typ = builder.getTokenType();
        final boolean res = !(TOPLEVEL_RECOVER_TYPES.contains(typ) || builder.eof());
        System.out.println(
            "toplevel_recover_debug: res: " + res
              + "; offset: " + builder.getCurrentOffset()
              + "; type: " + typ
              + "; text: '" + builder.getTokenText() + "'");
        return res;
    }
    // private toplevel_recover ::= !(<<eof>> | semi | "foreign" | "import" | "type" | "class" | "data" | "newtype" | "deriving")
    private static final java.util.Set<IElementType> TOPLEVEL_RECOVER_TYPES;
    static {
        TOPLEVEL_RECOVER_TYPES = new java.util.HashSet<>(Arrays.asList(
            HaskellTypes.SEMICOLON, HaskellTypes.WHITESPACESEMITOK,
            HaskellTypes.FOREIGNDECL,
            HaskellTypes.IMPORT,
            HaskellTypes.TYPE,
            HaskellTypes.CLASSTOKEN,
            HaskellTypes.DATA,
            HaskellTypes.NEWTYPE,
            HaskellTypes.DERIVING
        ));
    }

    /**
     * External rule used to determine if the current lexer position is
     * in an indent. Semantically, this returns true unless the previous
     * character was a newline and the current one is not whitespace.
     * Via recoverWhile, this rule will be used to consume input until
     * it reaches an unindented token, presumably a top-level element
     * of some sort, and resume at that time. It's quite convenient to just
     * resume parsing once we encounter a new top-level element.
     */
    public static boolean inIndentRecover(@NotNull PsiBuilder builder, int level) {
        if (builder.eof()) return false;
        final int offset = builder.getCurrentOffset();
        if (offset == 0) return false;
        final CharSequence text = builder.getOriginalText();
        return !(
            text.charAt(offset - 1) == '\n'
                && !Character.isWhitespace(text.charAt(offset)));
    }

    public static boolean anyNonSemi(@NotNull PsiBuilder builder, int level) {
        return
            !builder.eof() && (
                builder.getTokenType() == HaskellTypes.SEMICOLON
                  || builder.getTokenType() == HaskellTypes.WHITESPACESEMITOK);
    }

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

        // IElementType tok = builder.getTokenType();
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

    public static boolean indented(@NotNull PsiBuilder builder, int level, Parser geq) {
        if (!(builder instanceof Builder)) return false;
        PsiParser wrapper = ((Builder) builder).parser;
        if (!(wrapper instanceof HaskellParserWrapper)) return false;
        if (builder.eof()) return false;

        // IElementType currtok = builder.getTokenType();
        Pair<Integer, IElementType> prevtok = previousElem(builder);
        if (prevtok == null) return true;

        int offs = builder.getCurrentOffset();
        int line = StringUtil.offsetToLineNumber(builder.getOriginalText(), offs);
        int prevline = StringUtil.offsetToLineNumber(builder.getOriginalText(), offs + prevtok.getFirst());
        if (prevline == line) return true;

        int thisLineStart = StringUtil.lineColToOffset(builder.getOriginalText(), line, 0);
        int prevLineStart = StringUtil.lineColToOffset(builder.getOriginalText(), prevline, 0);
        // CharSequence lineStuff = builder.getOriginalText().subSequence(thisLineStart, offs);
        CharSequence prevLineStuff = builder.getOriginalText().subSequence(prevLineStart, thisLineStart - 1);
        int indentation = indentationLevel(prevLineStuff);
        int myindentation = offs - thisLineStart;
        // String tokName = builder.getTokenText();

        boolean geqVal = geq.parse(builder, level);
        if (geqVal && myindentation >= indentation ||
                !geqVal && myindentation > indentation) return true;
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
