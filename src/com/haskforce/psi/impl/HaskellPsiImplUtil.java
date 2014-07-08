package com.haskforce.psi.impl;

import com.haskforce.psi.HaskellConid;
import com.haskforce.psi.HaskellQvarid;
import com.haskforce.psi.HaskellTycls;
import com.haskforce.psi.HaskellTycon;
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
    public static String getName(@NotNull HaskellConid o) {
        return o.getConidRegexp().getText();
    }

    @NotNull
    public static String getName(@NotNull HaskellTycon o) {
        return o.getConid().getName();
    }

    @NotNull
    public static String getName(@NotNull HaskellTycls o) {
        return o.getConid().getName();
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

    @NotNull
    public static PsiElement setName(@NotNull HaskellConid o, @NotNull String newName) {
        o.replace(HaskellElementFactory.createConidFromText(o.getProject(), newName));
        return o;
    }

    @NotNull
    public static PsiElement setName(@NotNull HaskellTycon o, @NotNull String newName) {
        o.replace(HaskellElementFactory.createTyconFromText(o.getProject(), newName));
        return o;
    }

    @NotNull
    public static PsiElement setName(@NotNull HaskellTycls o, @NotNull String newName) {
        o.replace(HaskellElementFactory.createTyclsFromText(o.getProject(), newName));
        return o;
    }
}
