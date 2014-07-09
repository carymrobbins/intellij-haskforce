package com.haskforce.psi.references;

import com.haskforce.HaskellLanguage;
import com.intellij.patterns.PsiElementPattern;
import com.intellij.psi.PsiNamedElement;
import com.intellij.psi.PsiReferenceContributor;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.psi.PsiReferenceRegistrar;

/**
 * Adds references so they later can be resolved.
 */
public class HaskellReferenceContributor extends PsiReferenceContributor {
    @Override
    public void registerReferenceProviders(PsiReferenceRegistrar registrar) {
        PsiElementPattern.Capture<PsiNamedElement> variableCapture =
                PlatformPatterns.psiElement(PsiNamedElement.class).withLanguage(HaskellLanguage.INSTANCE);
        registrar.registerReferenceProvider(variableCapture,
                new HaskellReferenceProvider());
    }
}
