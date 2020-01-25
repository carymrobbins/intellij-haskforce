package com.haskforce.resolve;

import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase;
import com.haskforce.psi.HaskellFile;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.CharsetToolkit;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import org.jetbrains.annotations.Nullable;

import java.io.File;
import java.util.Collection;

import static org.junit.Assert.assertNotEquals;

public abstract class HaskellResolveTestCase extends HaskellLightPlatformCodeInsightFixtureTestCase {
    protected PsiReference referencedElement;
    protected PsiElement resolvedElement;

    public HaskellResolveTestCase(String srcName) {
        super(srcName, srcName);
    }

    public HaskellResolveTestCase() {
        this("resolve");
    }

    @Override
    protected String getTestDataPath() {
        return FileUtil.join(super.getTestDataPath(), getTestName(false));
    }

    private Collection<File> getTestDataFiles() {
        return com.haskforce.utils.FileUtil.findFilesRecursively(new File(getTestDataPath()));
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
            if (referencedOffset == -1 && resolvedOffset == -1) continue;
            String relativePath = file.getCanonicalPath().substring(
              file.getCanonicalPath().indexOf(getTestDataPath()) + getTestDataPath().length() + 1
            );
            myFixture.getTempDirFixture().createFile(relativePath, text);
            PsiFile psiFile = myFixture.configureFromTempProjectFile(relativePath);
//            PsiFile psiFile = myFixture.configureByText(relativePath, text);
            assertInstanceOf(psiFile, HaskellFile.class);
            if (referencedOffset != -1) {
                referencedElement = psiFile.findReferenceAt(referencedOffset);
                if (referencedElement == null) fail("Reference was null in " + file.getName());
            }
            if (resolvedOffset != -1) {
                final PsiReference ref = psiFile.findReferenceAt(resolvedOffset);
                if (ref == null) { fail("Reference was null in " + file.getName()); }
                resolvedElement = ref.getElement();
            }
        }
    }

    protected void doTest() { doTest(true); }

    protected void doTest(boolean succeed) {
        if (succeed && referencedElement == null) { fail("Could not find reference at caret."); }
        if (succeed && resolvedElement == null) { fail("Could not find resolved element."); }
        if (succeed) {
            assertEquals(
              "Failed to resolve reference",
              new PrettyPsiElementWrapper(resolvedElement),
              new PrettyPsiElementWrapper(referencedElement.resolve())
            );
        } else {
            assertNotEquals(
              "Resolved unexpected reference",
              new PrettyPsiElementWrapper(resolvedElement),
              new PrettyPsiElementWrapper(referencedElement.resolve())
            );
        }
    }

    /** Hack to improve assertion messages comparing elements. */
    private static class PrettyPsiElementWrapper {
        private final @Nullable PsiElement element;

        PrettyPsiElementWrapper(@Nullable PsiElement element) {
            this.element = element;
        }

        @Override
        public String toString() {
            if (element == null) return "null";
            return element.toString() + " [text = \"" + element.getText() + "\"]";
        }
    }
}
