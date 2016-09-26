package com.haskforce.haskell.psi.references;

import com.haskforce.haskell.HaskellLanguage;
import com.haskforce.haskell.psi.references.HaskellReference;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiReference;
import com.intellij.psi.PsiReferenceProvider;
import com.intellij.util.ProcessingContext;
import org.jetbrains.annotations.NotNull;

/**
 * Injects additional references into elements that supports
 * reference contributors
 */
public class HaskellReferenceProvider extends PsiReferenceProvider {
    @NotNull
    @Override
    public PsiReference[] getReferencesByElement(@NotNull PsiElement element,
                                                 @NotNull ProcessingContext context) {
        if (!element.getLanguage().is(HaskellLanguage.INSTANCE)) {
            return PsiReference.EMPTY_ARRAY;
        }

        if (element instanceof PsiNamedElement) {
            PsiNamedElement se = (PsiNamedElement) element;
            return new PsiReference[]{new HaskellReference(se, se.getTextRange())};
        }
        return PsiReference.EMPTY_ARRAY;
    }
}
