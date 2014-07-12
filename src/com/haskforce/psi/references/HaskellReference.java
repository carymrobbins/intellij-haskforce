package com.haskforce.psi.references;

import com.haskforce.HaskellIcons;
import com.haskforce.utils.HaskellUtil;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiElementResolveResult;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiPolyVariantReference;
import com.intellij.psi.PsiReferenceBase;
import com.intellij.psi.ResolveResult;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.List;

/**
 * Resolves references to elements.
 */
public class HaskellReference extends PsiReferenceBase<PsiElement> implements PsiPolyVariantReference {
    private String name;

    public HaskellReference(@NotNull PsiNamedElement element, TextRange textRange) {
        super(element, textRange);
        name = element.getName();
    }

    @NotNull
    @Override
    public ResolveResult[] multiResolve(boolean incompleteCode) {
        Project project = myElement.getProject();
        final List<PsiNamedElement> namedElements = HaskellUtil.findDefinitionNode(project, name);
        // Guess 20 variants tops most of the time in any real code base.
        List<ResolveResult> results = new ArrayList<ResolveResult>(20);
        for (PsiNamedElement property : namedElements) {
            //noinspection ObjectAllocationInLoop
            results.add(new PsiElementResolveResult(property));
        }
        return results.toArray(new ResolveResult[results.size()]);
    }

    @Nullable
    @Override
    public PsiElement resolve() {
        ResolveResult[] resolveResults = multiResolve(false);
        return resolveResults.length == 1 ? resolveResults[0].getElement() : null;
    }

    /**
     * Controls what names that get added to the autocompletion popup available
     * on ctrl-space.
     */
    @NotNull
    @Override
    public Object[] getVariants() {
        Project project = myElement.getProject();
        List<PsiNamedElement> namedNodes = HaskellUtil.findDefinitionNodes(project);
        List<LookupElement> variants = new ArrayList<LookupElement>(20);
        for (final PsiNamedElement namedElement : namedNodes) {
            if (namedElement.getName() != null && !namedElement.getName().isEmpty()) {
                variants.add(LookupElementBuilder.create(namedElement).
                                withIcon(HaskellIcons.FILE).
                                withTypeText(namedElement.getContainingFile().getName())
                );
            }
        }
        return variants.toArray();
    }

    @Override
    public TextRange getRangeInElement() {
        return new TextRange(0, this.getElement().getNode().getTextLength());
    }
}
