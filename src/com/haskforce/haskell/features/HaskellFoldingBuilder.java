package com.haskforce.haskell.features;

import com.haskforce.haskell.HaskellParserDefinition;
import com.haskforce.haskell.psi.HaskellFile;
import com.haskforce.haskell.psi.HaskellTypes;
import com.intellij.lang.ASTNode;
import com.intellij.lang.folding.CustomFoldingBuilder;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.DumbAware;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.TokenType;
import com.intellij.psi.search.PsiElementProcessor;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;

import java.util.List;
import java.util.Set;

/**
 * Collapses constructs into something small, such as "{- -}".
 */
public class HaskellFoldingBuilder extends CustomFoldingBuilder implements DumbAware {
    @Override
    protected void
    buildLanguageFoldRegions(@NotNull final List<FoldingDescriptor> descriptors,
                                            @NotNull PsiElement root,
                                            @NotNull Document document,
                                            boolean quick) {
        if (!(root instanceof HaskellFile)) return;
        HaskellFile file = (HaskellFile) root;
        final Set<PsiElement> seenComments = ContainerUtil.newHashSet();

        if (!quick) {
            PsiTreeUtil.processElements(file, new PsiElementProcessor() {
                @Override
                public boolean execute(@NotNull PsiElement element) {
                    if (element.getNode().getElementType().equals(HaskellTypes.COMMENT)) {
                        addCommentFolds((PsiComment) element, seenComments, descriptors);
                    } else if (HaskellParserDefinition.COMMENTS.contains(element.getNode().getElementType())) {
                        TextRange range = element.getTextRange();
                        String placeholderText = getPlaceholderText(element.getNode());
                        // Only fold if we actually save space to prevent
                        // assertions from kicking in. Means {- -} will not fold.
                        if (placeholderText != null && range.getLength() > 1 &&
                                range.getLength() > placeholderText.length()) {
                            descriptors.add(new FoldingDescriptor(element, range));
                        }
                    }
                    return true;
                }
            });
        }
    }

    /**
     * Provides the text displayed on folded elements.
     */
    @Override
    protected String
    getLanguagePlaceholderText(@NotNull ASTNode node, @NotNull TextRange range) {
        IElementType type = node.getElementType();
        if (HaskellTypes.OPENCOM.equals(type)) return "{-";
        // Need two character placeholder for hoovering to work.
        if (HaskellTypes.COMMENTTEXT.equals(type)) return "  ";
        if (HaskellTypes.CLOSECOM.equals(type)) return "-}";
        if (HaskellTypes.COMMENT.equals(type)) return "--";
        return "..";
    }

    /**
     * The default collapsed state for a folding region related to a node.
     */
    @Override
    protected boolean isRegionCollapsedByDefault(@NotNull ASTNode node) {
        return false;
    }

    /**
     * Single out nodes that we custom fold. Comment is the only current
     * element.
     */
    @Override
    protected boolean isCustomFoldingCandidate(@NotNull ASTNode node) {
        return HaskellTypes.COMMENT.equals(node.getElementType());
    }

    // Cut and paste from JavaFoldingBuilderEx.
    private static void
    addCommentFolds(@NotNull PsiComment c,
                    @NotNull Set<PsiElement> processedComments,
                    @NotNull List<FoldingDescriptor> foldElements) {
        if (processedComments.contains(c) ||
                !HaskellTypes.COMMENT.equals(c.getTokenType())) {
            return;
        }

        PsiElement end = null;
        for (PsiElement curr = c.getNextSibling(); curr != null; curr = curr.getNextSibling()) {
            ASTNode node = curr.getNode();
            if (node == null) {
                break;
            }
            IElementType elementType = node.getElementType();
            if (HaskellTypes.COMMENT.equals(elementType)) {
                end = curr;
                // We don't want to process, say, the second comment in case of
                // three subsequent comments when it's being examined during all
                // elements traversal. I.e. we expect to start from the first
                // comment and grab as many subsequent comments as possible
                // during the single iteration.
                processedComments.add(curr);
                continue;
            }
            if (TokenType.WHITE_SPACE.equals(elementType)) {
                continue;
            }
            break;
        }

        if (end != null) {
            TextRange rng = new TextRange(c.getTextRange().getStartOffset(),
                                          end.getTextRange().getEndOffset());
            foldElements.add(new FoldingDescriptor(c, rng));
        }
    }
}
