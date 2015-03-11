package com.haskforce.psi.references;

import com.haskforce.HaskellLanguage;
import com.haskforce.cabal.CabalLanguage;
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
public class CabalReferenceProvider extends PsiReferenceProvider {
    @NotNull
    @Override
    public PsiReference[] getReferencesByElement(@NotNull PsiElement element,
                                                 @NotNull ProcessingContext context) {
        if (!element.getLanguage().is(CabalLanguage.INSTANCE)) {
            return PsiReference.EMPTY_ARRAY;
        }

        if (element instanceof PsiNamedElement) {
            PsiNamedElement se = (PsiNamedElement) element;
            return new PsiReference[]{new CabalReference(se, se.getTextRange())};
        }
        return PsiReference.EMPTY_ARRAY;
    }
}
