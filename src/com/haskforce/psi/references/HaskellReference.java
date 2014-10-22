package com.haskforce.psi.references;

import com.haskforce.HaskellIcons;
import com.haskforce.highlighting.annotation.external.GhcModi;
import com.haskforce.psi.HaskellConid;
import com.haskforce.psi.HaskellExp;
import com.haskforce.psi.HaskellGendecl;
import com.haskforce.psi.HaskellVarid;
import com.haskforce.psi.impl.HaskellPsiImplUtil;
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
import com.intellij.util.IncorrectOperationException;
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

    /**
     * Resolves references to a set of results.
     */
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

    /**
     * Resolves references to a single result, or fails.
     */
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
        // If we are not in an expression, don't provide references.
        PsiElement el;
        if ((el = myElement.getParent()) == null
                || (el = el.getParent()) == null
                || !(el.getParent() instanceof HaskellExp)) {
            return new Object[]{};
        }
        Project project = myElement.getProject();
        List<PsiNamedElement> namedNodes = HaskellUtil.findDefinitionNodes(project);
        List<LookupElement> variants = new ArrayList<LookupElement>(20);
        for (final PsiNamedElement namedElement : namedNodes) {
            if ((el = namedElement.getParent()) != null
                    && (el = el.getParent()) instanceof HaskellGendecl) {
                final String[] parts = GhcModi.TYPE_SPLIT_REGEX.split(el.getText(), 2);
                final String moduleName = " (" + namedElement.getContainingFile().getName() + ')';
                if (parts.length == 2) {
                    variants.add(LookupElementBuilder.create(parts[0])
                                    .withIcon(HaskellIcons.FILE)
                                    .withTailText(moduleName, true)
                                    .withTypeText(parts[1]));
                    continue;
                }
            }
            final String name = namedElement.getName();
            if (name != null && !name.isEmpty()) {
                final String moduleName = " (" + namedElement.getContainingFile().getName() + ')';
                variants.add(LookupElementBuilder.create(namedElement)
                                .withIcon(HaskellIcons.FILE)
                                .withTailText(moduleName, true)
                );
            }
        }
        return variants.toArray();
    }

    @Override
    public TextRange getRangeInElement() {
        return new TextRange(0, this.getElement().getNode().getTextLength());
    }

    /**
     * Called when renaming refactoring tries to rename the Psi tree.
     */
    @Override
    public PsiElement handleElementRename(final String newName)  throws IncorrectOperationException {
        PsiElement element;
        if (myElement instanceof HaskellVarid) {
            element = HaskellPsiImplUtil.setName((HaskellVarid) myElement, newName);
            if (element != null) return element;
            throw new IncorrectOperationException("Cannot rename " + name + " to " + newName);
        } else if (myElement instanceof HaskellConid) {
            element = HaskellPsiImplUtil.setName((HaskellConid) myElement, newName);
            if (element != null) return element;
            throw new IncorrectOperationException("Cannot rename " + name + " to " + newName);
        }
        return super.handleElementRename(newName);
    }
}
