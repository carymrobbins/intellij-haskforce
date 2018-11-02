package com.haskforce.psi.impl;

import com.haskforce.HaskellLanguage;
import com.haskforce.psi.*;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Performs creation of element types.
 */
public class HaskellElementFactory {
    /**
     * Takes a name and returns a Psi node of that name, or null.
     */
    @Nullable
    public static HaskellVarid createVaridFromText(@NotNull Project project, @NotNull String name) {
        PsiElement e = createExpressionFromText(project, name + "uniq = " + name).getFirstChild();
        if (e instanceof HaskellVarid) return (HaskellVarid) e;
        return null;
    }

    /**
     * Takes a name and returns a Psi node of that name, or null.
     */
    @NotNull
    public static HaskellQvarid createQvaridFromText(@NotNull Project project, @NotNull String name) {
        return ((HaskellQvarid) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    /**
     * Takes a name and returns a Psi node of that name, or null.
     */
    @Nullable
    public static HaskellConid createConidFromText(@NotNull Project project, @NotNull String name) {
        PsiElement e = createExpressionFromText(project, name + "uniq = " + name).getFirstChild();
        if (e instanceof HaskellConid) return (HaskellConid) e;
        return null;
    }

    /**
     * Takes a name and returns a Psi node of that name, or null.
     */
    @NotNull
    public static HaskellTycon createTyconFromText(@NotNull Project project, @NotNull String name) {
        return ((HaskellTycon) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    /**
     * Takes a name and returns a Psi node of that name, or null.
     */
    @NotNull
    public static HaskellTycls createTyclsFromText(@NotNull Project project, @NotNull String name) {
        return ((HaskellTycls) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    @NotNull
    public static HaskellPpragma createPpragmaFromText(@NotNull Project project, @NotNull String text) {
        return ((HaskellPpragma) (createFileFromText(project, text + "\nmodule Foo where").getFirstChild()));
    }

    @NotNull
    public static HaskellGendecl createGendeclFromText(@NotNull Project project, @NotNull String text) {
        return ((HaskellGendecl) (createFileFromText(project, text).getFirstChild().getFirstChild()));
    }

    @NotNull
    public static HaskellImpdecl createImpdeclFromText(@NotNull Project project, @NotNull String text) {
        return ((HaskellImpdecl) (createFileFromText(project, text).getFirstChild().getFirstChild()));
    }

    @Nullable
    public static HaskellCtype createCtypeFromText(@NotNull Project project, @NotNull String text) {
        return PsiTreeUtil.findChildOfType(createFileFromText(project, "foo :: " + text), HaskellCtype.class);
    }

    @NotNull
    public static PsiWhiteSpace createNewLine(@NotNull Project project) {
        return ((PsiWhiteSpace) (createFileFromText(project, "\n")).getFirstChild());
    }

    @NotNull
    public static PsiWhiteSpace createSpace(@NotNull Project project) {
        return ((PsiWhiteSpace) (createFileFromText(project, " ")).getFirstChild());
    }

    @NotNull
    public static PsiElement createComma(@NotNull Project project) {
        HaskellFile fileFromText = createFileFromText(project, "x = (,)");
        PsiElement rhs = fileFromText.getFirstChild().getFirstChild().getLastChild();
        PsiElement comma = rhs.getLastChild().getFirstChild().getNextSibling();
        return comma;
    }

    /**
     * Takes an expression in text and returns a Psi tree of that program.
     */
    @NotNull
    public static PsiElement createExpressionFromText(@NotNull Project project, @NotNull String name) {
        HaskellFile fileFromText = createFileFromText(project, name);
        PsiElement rhs = fileFromText.getFirstChild().getFirstChild().getLastChild();
        PsiElement nodeOfInterest = rhs.getLastChild().getLastChild().getLastChild();
        return nodeOfInterest;
    }

    /**
     * Create a file containing text.
     */
    @NotNull
    public static HaskellFile createFileFromText(@NotNull Project project, @NotNull String text) {
        return (HaskellFile) PsiFileFactory.getInstance(project).createFileFromText("A.hs", HaskellLanguage.INSTANCE, text);
    }
}
