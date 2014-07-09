package com.haskforce.psi.impl;

import com.haskforce.psi.references.HaskellReference;
import com.haskforce.psi.HaskellConid;
import com.haskforce.psi.HaskellVarid;
import com.intellij.openapi.util.TextRange;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiReference;
import org.jetbrains.annotations.NotNull;

/**
 * Source of the methods pointed out in Haskell.bnf.
 */
public class HaskellPsiImplUtil {

    @NotNull
    public static String getName(@NotNull HaskellVarid o) {
        return o.getVaridRegexp().getText();
    }

    @NotNull
    public static String getName(@NotNull HaskellConid o) {
        return o.getConidRegexp().getText();
    }

    @NotNull
    public static PsiElement setName(@NotNull HaskellVarid o, @NotNull String newName) {
        o.replace(HaskellElementFactory.createVaridFromText(o.getProject(), newName));
        return o;
    }

    @NotNull
    public static PsiElement setName(@NotNull HaskellConid o, @NotNull String newName) {
        o.replace(HaskellElementFactory.createConidFromText(o.getProject(), newName));
        return o;
    }

    @NotNull
    public static PsiReference getReference(@NotNull HaskellVarid o) {
        return new HaskellReference(o, TextRange.from(0, getName(o).length()));
    }

    @NotNull
    public static PsiReference getReference(@NotNull HaskellConid o) {
        return new HaskellReference(o, TextRange.from(0, getName(o).length()));
    }
}
