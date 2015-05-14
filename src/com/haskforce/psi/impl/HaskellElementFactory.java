package com.haskforce.psi.impl;

import com.haskforce.HaskellLanguage;
import com.haskforce.psi.*;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.List;

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
    @Nullable
    public static HaskellQconid createQconidFromText(@NotNull Project project, List<String> dirs, String moduleName) {
        dirs.add(moduleName);
        String moduleDeclaration = "module " + StringUtils.join(dirs, ".") + " where";
        HaskellFile fileFromText = createFileFromText(project, moduleDeclaration);
        HaskellQconid haskellQconid = PsiTreeUtil.findChildOfType(fileFromText, HaskellQconid.class);
        return haskellQconid;
    }


    public static PsiElement createQconidFromText(Project project, String newName) {
        String moduleDeclaration = "module " + newName + " where";
        HaskellFile fileFromText = createFileFromText(project, moduleDeclaration);
        HaskellQconid haskellQconid = PsiTreeUtil.findChildOfType(fileFromText, HaskellQconid.class);
        return haskellQconid;
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
    public static PsiWhiteSpace createNewLine(@NotNull Project project) {
        return ((PsiWhiteSpace) (createFileFromText(project, "\n")).getFirstChild());
    }

    @NotNull
    public static PsiElement createDot(@NotNull Project project) {
        HaskellFile fileFromText = createFileFromText(project, "import A.B");
        HaskellConid a = PsiTreeUtil.findChildOfType(fileFromText, HaskellConid.class);
        PsiElement dot = a.getNextSibling();
        return  dot;
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
