package com.haskforce.features;

import com.haskforce.psi.HaskellTypes;
import com.intellij.lang.ASTNode;
import com.intellij.lang.CodeDocumentationAwareCommenterEx;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.tree.IElementType;
import org.jetbrains.annotations.Nullable;

/**
 * Provides the two menu choices "Comment with .." in the Code menu.
 */
public class HaskellCommenter implements CodeDocumentationAwareCommenterEx {

    public static final String HADDOCKPREFIX = "-- |";

    @Override
    public boolean isDocumentationCommentText(PsiElement psiElement) {
        if (psiElement == null) {
            return false;
        }
        final ASTNode node = psiElement.getNode();
        return node != null &&
                (node.getElementType() == getDocumentationCommentTokenType());
    }

    @Nullable
    @Override
    public IElementType getLineCommentTokenType() {
        return HaskellTypes.COMMENT;
    }

    @Nullable
    @Override
    public IElementType getBlockCommentTokenType() {
        return HaskellTypes.OPENCOM;
    }

    @Nullable
    @Override
    public IElementType getDocumentationCommentTokenType() {
        return HaskellTypes.HADDOCK;
    }

    /**
     * The Haddock prefix ("-- |").
     */
    @Nullable
    @Override
    public String getDocumentationCommentPrefix() {
        return HADDOCKPREFIX;
    }

    /**
     * The prefix for multiline Haddock comments ("--").
     */
    @Nullable
    @Override
    public String getDocumentationCommentLinePrefix() {
        return "--";
    }

    /**
     * The suffix for Haddock (null).
     */
    @Nullable
    @Override
    public String getDocumentationCommentSuffix() {
        return null;
    }

    /**
     * Checks whether the comment starts with the haddock prefix.
     */
    @Override
    public boolean isDocumentationComment(PsiComment psiComment) {
        return psiComment.getText().startsWith(HADDOCKPREFIX);
    }

    /**
     * The prefix for line comments ("--").
     */
    @Nullable
    @Override
    public String getLineCommentPrefix() {
        return "--";
    }

    /**
     * The prefix for block comments ("{-").
     */
    @Nullable
    @Override
    public String getBlockCommentPrefix() {
        return "{-";
    }

    /**
     * The suffix for block comments ("-}").
     */
    @Nullable
    @Override
    public String getBlockCommentSuffix() {
        return "-}";
    }

    @Nullable
    @Override
    public String getCommentedBlockCommentPrefix() {
        return "{-";
    }

    @Nullable
    @Override
    public String getCommentedBlockCommentSuffix() {
        return "-}";
    }
}
