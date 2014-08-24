package com.haskforce.features;

import com.haskforce.HaskellParserDefinition;
import com.haskforce.psi.HaskellTypes;
import com.intellij.lang.BracePair;
import com.intellij.lang.PairedBraceMatcher;
import com.intellij.psi.PsiFile;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.tree.TokenSet;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;


/**
 * Utility class that relates opening and closing "braces".
 */
public class HaskellBraceMatcher implements PairedBraceMatcher {
    private static final BracePair[] PAIRS = new BracePair[]{
            new BracePair(HaskellTypes.LPAREN, HaskellTypes.RPAREN, false),
            new BracePair(HaskellTypes.LBRACE, HaskellTypes.RBRACE, true),
            new BracePair(HaskellTypes.LBRACKET, HaskellTypes.RBRACKET, false),
            new BracePair(HaskellTypes.OPENCOM, HaskellTypes.CLOSECOM, true),
            new BracePair(HaskellTypes.OPENPRAGMA, HaskellTypes.CLOSEPRAGMA, true),
            new BracePair(HaskellTypes.PARENSPLICE, HaskellTypes.RPAREN, true),
            new BracePair(HaskellTypes.LTHOPEN, HaskellTypes.RTHCLOSE, true),
            new BracePair(HaskellTypes.LBRACKET, HaskellTypes.RTHCLOSE, true)
    };

    private static final TokenSet alwaysMatch = TokenSet.create(HaskellTypes.LBRACE, HaskellTypes.OPENCOM, HaskellTypes.OPENPRAGMA);

    @Override
    public BracePair[] getPairs() {
        return PAIRS;
    }

    @Override
    public boolean isPairedBracesAllowedBeforeType(@NotNull IElementType lbraceType, @Nullable IElementType contextType) {
        return alwaysMatch.contains(lbraceType)
                || HaskellParserDefinition.WHITE_SPACES.contains(contextType)
                || HaskellParserDefinition.COMMENTS.contains(contextType)
                || HaskellTypes.RPAREN == contextType
                || null == contextType;
    }

    @Override
    public int getCodeConstructStart(PsiFile file, int openingBraceOffset) {
        return openingBraceOffset;
    }
}
