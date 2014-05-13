package com.haskforce.psi.impl;

import com.haskforce.HaskellLanguage;
import com.haskforce.psi.HaskellFile;
import com.haskforce.psi.HaskellQvarid;
import com.haskforce.psi.HaskellVarid;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFileFactory;
import org.jetbrains.annotations.NotNull;

/**
 * Performs creation of element types.
 */
public class HaskellElementFactory {
    @NotNull
    public static HaskellVarid createVaridFromText(@NotNull Project project, @NotNull String text) {
        return ((HaskellVarid) (createExpressionFromText(project, text + " = 2")).getFirstChild());
    }

    @NotNull
    public static HaskellQvarid createQvaridFromText(@NotNull Project project, @NotNull String text) {
        return ((HaskellQvarid) (createExpressionFromText(project, text + " = 2")).getFirstChild());
    }

    @NotNull
    public static PsiElement createExpressionFromText(@NotNull Project project, @NotNull String text) {
        HaskellFile fileFromText = createFileFromText(project, text);
        return fileFromText.getFirstChild().getFirstChild();
    }

    @NotNull
    private static HaskellFile createFileFromText(@NotNull Project project, @NotNull String text) {
        return (HaskellFile) PsiFileFactory.getInstance(project).createFileFromText("A.hs", HaskellLanguage.INSTANCE, text);
    }
}
