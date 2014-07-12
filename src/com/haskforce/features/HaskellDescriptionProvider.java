package com.haskforce.features;

import com.haskforce.HaskellLanguage;
import com.intellij.psi.ElementDescriptionLocation;
import com.intellij.psi.ElementDescriptionProvider;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiNamedElement;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Necessary for displaying the results of HaskellFindUsagesProvider.
 */
public class HaskellDescriptionProvider implements ElementDescriptionProvider {
    @Nullable
    @Override
    public String getElementDescription(@NotNull PsiElement element, @NotNull ElementDescriptionLocation location) {
        if (!element.getLanguage().is(HaskellLanguage.INSTANCE)) return null;
        if (!(element instanceof PsiNamedElement)) return null;
        return ((PsiNamedElement) element).getName();
    }
}
