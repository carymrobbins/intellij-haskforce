package com.haskforce.psi;

import com.haskforce.parser.HaskellParser;
import com.intellij.lang.ASTNode;
import com.intellij.lang.ITokenTypeRemapper;
import com.intellij.lang.PsiBuilder;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

/**
 * Wraps the entry-point for the Grammar-Kit parser to register
 * a token-remapper.
 */
public class HaskellParserWrapper extends HaskellParser {
    final static ITokenTypeRemapper myRemapper = new ITokenTypeRemapper() {
        /**
         * Intercept varsymplus tokens and correct them.
         */
        @Override
        public IElementType filter(IElementType source, int start, int end, CharSequence text) {
            if (!HaskellTypes.VARSYMTOKPLUS.equals(source)) return source;

            String token = text.toString();
            if (HaskellParserUtilBase.HASKELL_RESERVEDOP.contains(token)) {
                return HaskellTypes.RESERVEDOP;
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
        return super.parse(root_, builder_);
    }
}
