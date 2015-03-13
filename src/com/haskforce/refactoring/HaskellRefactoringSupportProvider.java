package com.haskforce.refactoring;

import com.haskforce.psi.HaskellNamedElement;
import com.intellij.lang.refactoring.RefactoringSupportProvider;
import com.intellij.psi.PsiElement;
import org.jetbrains.annotations.NotNull;

/**
 * Allows/enables in-place refactoring.
 */
public class HaskellRefactoringSupportProvider extends RefactoringSupportProvider {
    @Override
    public boolean isMemberInplaceRenameAvailable(@NotNull PsiElement element, PsiElement context) {
        return element instanceof HaskellNamedElement;
    }


}
