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
        return HaskellTypes.NCOMMENT;
    }

    @Nullable
    @Override
    public IElementType getDocumentationCommentTokenType() {
        return HaskellTypes.HADDOCK;
    }

    @Nullable
    @Override
    public String getDocumentationCommentPrefix() {
        return null;
    }

    @Nullable
    @Override
    public String getDocumentationCommentLinePrefix() {
        return "-- ^";
    }

    @Nullable
    @Override
    public String getDocumentationCommentSuffix() {
        return null;
    }

    @Override
    public boolean isDocumentationComment(PsiComment psiComment) {
        return false;
    }

    @Nullable
    @Override
    public String getLineCommentPrefix() {
        return "--";
    }

    @Nullable
    @Override
    public String getBlockCommentPrefix() {
        return "{-";
    }

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
