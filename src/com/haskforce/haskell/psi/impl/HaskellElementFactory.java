package com.haskforce.haskell.psi.impl;

import com.haskforce.haskell.HaskellLanguage;
import com.haskforce.haskell.psi.*;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.PsiWhiteSpace;
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
    public static com.haskforce.haskell.psi.HaskellVarid createVaridFromText(@NotNull Project project, @NotNull String name) {
        PsiElement e = createExpressionFromText(project, name + "uniq = " + name).getFirstChild();
        if (e instanceof com.haskforce.haskell.psi.HaskellVarid) return (com.haskforce.haskell.psi.HaskellVarid) e;
        return null;
    }

    /**
     * Takes a name and returns a Psi node of that name, or null.
     */
    @NotNull
    public static com.haskforce.haskell.psi.HaskellQvarid createQvaridFromText(@NotNull Project project, @NotNull String name) {
        return ((com.haskforce.haskell.psi.HaskellQvarid) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    /**
     * Takes a name and returns a Psi node of that name, or null.
     */
    @Nullable
    public static com.haskforce.haskell.psi.HaskellConid createConidFromText(@NotNull Project project, @NotNull String name) {
        PsiElement e = createExpressionFromText(project, name + "uniq = " + name).getFirstChild();
        if (e instanceof com.haskforce.haskell.psi.HaskellConid) return (com.haskforce.haskell.psi.HaskellConid) e;
        return null;
    }

    /**
     * Takes a name and returns a Psi node of that name, or null.
     */
    @NotNull
    public static com.haskforce.haskell.psi.HaskellTycon createTyconFromText(@NotNull Project project, @NotNull String name) {
        return ((com.haskforce.haskell.psi.HaskellTycon) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    /**
     * Takes a name and returns a Psi node of that name, or null.
     */
    @NotNull
    public static com.haskforce.haskell.psi.HaskellTycls createTyclsFromText(@NotNull Project project, @NotNull String name) {
        return ((com.haskforce.haskell.psi.HaskellTycls) (createExpressionFromText(project, name + "uniq = " + name)).getFirstChild());
    }

    @NotNull
    public static com.haskforce.haskell.psi.HaskellPpragma createPpragmaFromText(@NotNull Project project, @NotNull String text) {
        return ((com.haskforce.haskell.psi.HaskellPpragma) (createFileFromText(project, text + "\nmodule Foo where").getFirstChild()));
    }

    @NotNull
    public static com.haskforce.haskell.psi.HaskellGendecl createGendeclFromText(@NotNull Project project, @NotNull String text) {
        return ((com.haskforce.haskell.psi.HaskellGendecl) (createFileFromText(project, text).getFirstChild().getFirstChild()));
    }

    @NotNull
    public static PsiWhiteSpace createNewLine(@NotNull Project project) {
        return ((PsiWhiteSpace) (createFileFromText(project, "\n")).getFirstChild());
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
