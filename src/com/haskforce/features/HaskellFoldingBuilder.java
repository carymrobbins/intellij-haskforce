package com.haskforce.features;

import com.haskforce.HaskellParserDefinition;
import com.haskforce.parsing.HaskellTypes2;
import com.haskforce.psi.HaskellFile;
import com.haskforce.psi.HaskellTypes;
import com.intellij.lang.ASTNode;
import com.intellij.lang.folding.FoldingBuilderEx;
import com.intellij.lang.folding.FoldingDescriptor;
import com.intellij.openapi.editor.Document;
import com.intellij.openapi.project.DumbAware;
import com.intellij.psi.PsiElement;
import com.intellij.psi.search.PsiElementProcessor;
import com.intellij.psi.tree.IElementType;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * Collapses constructs into something small, such as "{- —}".
 */
public class HaskellFoldingBuilder extends FoldingBuilderEx implements DumbAware {
    @NotNull
    @Override
    public FoldingDescriptor[] buildFoldRegions(@NotNull PsiElement root, @NotNull Document document, boolean quick) {
        if (!(root instanceof HaskellFile)) return FoldingDescriptor.EMPTY;
        HaskellFile file = (HaskellFile) root;
        final List<FoldingDescriptor> result = ContainerUtil.newArrayList();

        if (!quick) {
            PsiTreeUtil.processElements(file, new PsiElementProcessor() {
                @Override
                public boolean execute(@NotNull PsiElement element) {
                    if (HaskellParserDefinition.COMMENTS.contains(element.getNode().getElementType())) {
                        result.add(new FoldingDescriptor(element, element.getTextRange()));
                    }
                    return true;
                }
            });
        }

        // TODO: See JavaFoldingBuilder.java for example of collecting
        // consecitive comments.
        return result.toArray(new FoldingDescriptor[result.size()]);
    }

    /**
     * Provides the text displayed on folded elements.
     */
    @Nullable
    @Override
    public String getPlaceholderText(@NotNull ASTNode node) {
        IElementType type = node.getElementType();
        if (type == HaskellTypes.COMMENTTEXT) return "{- -}";
        if (type == HaskellTypes.COMMENT) return "--";
        return "..";
    }

    @Override
    public boolean isCollapsedByDefault(@NotNull ASTNode node) {
        return false;
    }
}
