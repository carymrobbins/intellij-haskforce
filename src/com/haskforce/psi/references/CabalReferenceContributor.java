package com.haskforce.psi.references;

import com.haskforce.cabal.CabalLanguage;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.patterns.PsiElementPattern;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiReferenceContributor;
import com.intellij.psi.PsiReferenceRegistrar;

/**
 * Adds references so they later can be resolved.
 */
public class CabalReferenceContributor extends PsiReferenceContributor {
    @Override
    public void registerReferenceProviders(PsiReferenceRegistrar registrar) {
        PsiElementPattern.Capture<PsiNamedElement> variableCapture =
                PlatformPatterns.psiElement(PsiNamedElement.class).withLanguage(CabalLanguage.INSTANCE);
        registrar.registerReferenceProvider(variableCapture,
                new CabalReferenceProvider());
    }
}
