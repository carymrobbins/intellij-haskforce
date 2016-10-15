package com.haskforce.haskell.resolve;

import com.haskforce.haskell.HaskellLightPlatformCodeInsightFixtureTestCase;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.CharsetToolkit;
import com.intellij.openapi.vfs.VirtualFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;

import java.io.File;
import java.util.Collection;

public abstract class HaskellResolveTestCase extends HaskellLightPlatformCodeInsightFixtureTestCase {
    protected PsiReference referencedElement;
    protected PsiElement resolvedElement;

    public HaskellResolveTestCase(String srcName) {
        super(srcName, srcName);
    }

    public HaskellResolveTestCase(String srcName, boolean noHaskellBaseDirectory) {
        super(srcName, srcName, noHaskellBaseDirectory);
    }

    public HaskellResolveTestCase() {
        this("resolve");
    }

    @Override
    protected String getTestDataPath() {
        return FileUtil.join(super.getTestDataPath(), getTestName(false));
    }

    private Collection<File> getTestDataFiles() {
        return com.haskforce.system.utils.FileUtil.findFilesRecursively(new File(getTestDataPath()));
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        for (File file : getTestDataFiles()) {
            if (file.isDirectory()) continue;
            String text = FileUtil.loadFile(file, CharsetToolkit.UTF8);
            text = StringUtil.convertLineSeparators(text);
            int referencedOffset = text.indexOf("<ref>");
            text = text.replace("<ref>", "");
            int resolvedOffset = text.indexOf("<resolved>");
            text = text.replace("<resolved>", "");
            String relativePath = file.getCanonicalPath().substring(
              file.getCanonicalPath().indexOf(getTestDataPath()) + getTestDataPath().length() + 1
            );
            VirtualFile vFile = myFixture.getTempDirFixture().createFile(relativePath, text);
            PsiFile psiFile = myFixture.configureFromTempProjectFile(relativePath);
            if (referencedOffset != -1) {
                referencedElement = psiFile.findReferenceAt(referencedOffset);
                if (referencedElement == null) fail("Reference was null in " + file.getName());
            }
            if (resolvedOffset != -1) {
                final PsiReference ref = psiFile.findReferenceAt(resolvedOffset);
                if (ref == null) { fail("Reference was null in " + file.getName()); }
                resolvedElement = ref.getElement();
                if (resolvedElement == null) { fail("Reference returned null element in " + file.getName()); }
            }
        }
    }

    protected void doTest() { doTest(true); }

    protected void doTest(boolean succeed) {
        if (succeed && referencedElement == null) { fail("Could not find reference at caret."); }
        if (succeed && resolvedElement == null) { fail("Could not find resolved element."); }
        if (succeed) {
            PsiElement resolvedActual = referencedElement.resolve();
            assertEquals(
              "Could not resolve expected reference.\n" +
                "Expected: " + resolvedElement + " (" + resolvedElement.getText() + ")\n" +
                "Actual: " + resolvedActual + " (" + resolvedActual.getText() + ")",
              resolvedElement,
              resolvedActual
            );
        } else {
            assertFalse("Resolved unexpected reference.", resolvedElement.equals(referencedElement.resolve()));
        }
    }
}
