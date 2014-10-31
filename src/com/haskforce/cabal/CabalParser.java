package com.haskforce.cabal;

import com.intellij.lang.ASTNode;
import com.intellij.lang.PsiBuilder;
import com.intellij.lang.PsiParser;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.NotNull;

/**
 * Simple parser which basically returns the result from the syntax highlighting lexer.
 */
public class CabalParser implements PsiParser {
    @NotNull
    @Override
    public ASTNode parse(IElementType root, PsiBuilder builder) {
        PsiBuilder.Marker marker = builder.mark();
        while (!builder.eof()) {
            builder.advanceLexer();
        }
        marker.done(root);
        return builder.getTreeBuilt();
    }
}
