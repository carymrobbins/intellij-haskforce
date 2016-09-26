package com.haskforce.haskell.features;

import com.haskforce.haskell.psi.HaskellTypes;
import com.intellij.codeInsight.editorActions.SimpleTokenSetQuoteHandler;

/**
 * Enables overwriting the closing string quote so you can proceed
 * typing without having to move the cursor by hand.
 */
public class HaskellQuoteHandler extends SimpleTokenSetQuoteHandler {
    public HaskellQuoteHandler() {
        super(HaskellTypes.STRINGTOKEN, HaskellTypes.BADSTRINGTOKEN, HaskellTypes.DOUBLEQUOTE,
                HaskellTypes.CHARTOKEN, HaskellTypes.SINGLEQUOTE);
    }
}
