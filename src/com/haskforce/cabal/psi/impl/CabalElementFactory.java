package com.haskforce.cabal.psi.impl;

import com.haskforce.HaskellLanguage;
import com.haskforce.cabal.CabalLanguage;
import com.haskforce.cabal.psi.CabalDependency;
import com.haskforce.cabal.psi.CabalFile;
import com.haskforce.psi.HaskellFile;
import com.intellij.openapi.project.Project;
import com.intellij.psi.PsiFileFactory;
import org.jetbrains.annotations.NotNull;

public class CabalElementFactory {
    public static CabalDependency createCabalDependency(Project project, String packageName) {
        CabalFile fileFromText = createFileFromText(project, "library\nbuild-depends:\n" + packageName);
        return null;
    }

    /**
     * Create a file containing text.
     */
    @NotNull
    public static CabalFile createFileFromText(@NotNull Project project, @NotNull String text) {
        return (CabalFile) PsiFileFactory.getInstance(project).createFileFromText("A.cabal", CabalLanguage.INSTANCE, text);
    }
}
