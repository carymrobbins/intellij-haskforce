package com.haskforce.psi.impl;

import com.haskforce.HaskellLanguage;
import com.haskforce.psi.HaskellConid;
import com.haskforce.psi.HaskellFile;
import com.haskforce.psi.HaskellQvarid;
import com.haskforce.psi.HaskellTycls;
import com.haskforce.psi.HaskellTycon;
import com.haskforce.psi.HaskellVarid;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFileFactory;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

/**
 * Performs creation of element types.
 */
public class HaskellElementFactory {
    @Nullable
    public static HaskellVarid createVaridFromText(@NotNull Project project, @NotNull String text) {
        PsiElement e = createExpressionFromText(project, text + " = 2").getFirstChild();
        if (e instanceof HaskellVarid) return (HaskellVarid) e;
        return null;
    }

    @NotNull
    public static HaskellQvarid createQvaridFromText(@NotNull Project project, @NotNull String text) {
        return ((HaskellQvarid) (createExpressionFromText(project, text + " = 2")).getFirstChild());
    }

    @Nullable
    public static HaskellConid createConidFromText(@NotNull Project project, @NotNull String text) {
        PsiElement e = createExpressionFromText(project, text + " = 2").getFirstChild();
        if (e instanceof HaskellConid) return (HaskellConid) e;
        return null;
    }

    @NotNull
    public static HaskellTycon createTyconFromText(@NotNull Project project, @NotNull String text) {
        return ((HaskellTycon) (createExpressionFromText(project, text + " = 2")).getFirstChild());
    }

    @NotNull
    public static HaskellTycls createTyclsFromText(@NotNull Project project, @NotNull String text) {
        return ((HaskellTycls) (createExpressionFromText(project, text + " = 2")).getFirstChild());
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
