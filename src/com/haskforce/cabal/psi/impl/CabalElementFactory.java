package com.haskforce.cabal.psi.impl;

import com.haskforce.HaskellLanguage;
import com.haskforce.cabal.CabalLanguage;
import com.haskforce.cabal.psi.CabalBuildInformation;
import com.haskforce.cabal.psi.CabalDependency;
import com.haskforce.cabal.psi.CabalFile;
import com.haskforce.psi.HaskellFile;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFileFactory;
import com.intellij.psi.util.PsiTreeUtil;
import org.jetbrains.annotations.NotNull;

public class CabalElementFactory {
    public static CabalDependency createCabalDependency(Project project, String packageName) {
        String textToParse = "library\n  build-depends:\n    " + packageName;
        CabalFile fileFromText = createFileFromText(project, textToParse);
        return PsiTreeUtil.findChildOfType(fileFromText, CabalDependency.class);
    }

    public static CabalBuildInformation createCabalBuildInformation(Project project, String packageName) {
        String textToParse = "library\n  build-depends:\n    " + packageName;
        CabalFile fileFromText = createFileFromText(project, textToParse);
        return PsiTreeUtil.findChildOfType(fileFromText, CabalBuildInformation.class);
    }
    /**
     * Create a file containing text.
     */
    @NotNull
    public static CabalFile createFileFromText(@NotNull Project project, @NotNull String text) {
        return (CabalFile) PsiFileFactory.getInstance(project).createFileFromText("A.cabal", CabalLanguage.INSTANCE, text);
    }

}
