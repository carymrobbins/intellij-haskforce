package com.haskforce.psi.impl;

import com.haskforce.psi.HaskellQvarid;
import com.haskforce.psi.HaskellVarid;
import com.intellij.psi.PsiElement;
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
    public static String getName(@NotNull HaskellQvarid o) {
        return o.getVarid().getName();
    }

    @NotNull
    public static PsiElement setName(@NotNull HaskellVarid o, @NotNull String newName) {
        o.replace(HaskellElementFactory.createVaridFromText(o.getProject(), newName));
        return o;
    }

    @NotNull
    public static PsiElement setName(@NotNull HaskellQvarid o, @NotNull String newName) {
        o.replace(HaskellElementFactory.createQvaridFromText(o.getProject(), newName));
        return o;
    }
}
