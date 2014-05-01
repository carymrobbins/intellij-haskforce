package com.haskforce.highlighting;

import com.intellij.core.CoreApplicationEnvironment;
import com.intellij.lang.LanguageExtensionPoint;
import com.intellij.lexer.Lexer;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.openapi.util.Comparing;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.CharsetToolkit;
import com.intellij.rt.execution.junit.FileComparisonFailure;
import com.intellij.testFramework.LexerTestCase;
import com.intellij.testFramework.TestDataFile;
import com.intellij.testFramework.VfsTestUtil;
import org.jetbrains.annotations.NonNls;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;

public abstract class HaskellLexerTestBase extends LexerTestCase {
    private final String srcPath = getDirPath() + File.separator + "parser";
    private final String expectPath = getDirPath() + File.separator + "lexer";
    public HaskellLexerTestBase() {
        super();
    }

    public void doTest(boolean checkResult, boolean shouldPass) {
        String fileName = getTestName(false) + ".hs";
        String text = "";
        try {
            text = loadFile(fileName);
        } catch (IOException e) {
            fail("can't load file " + fileName + ": " + e.getMessage());
        }
        String result = printTokens(text, 0);
        try {
            doCheckResult(expectPath + File.separator + "expected",
                    getTestName(false) + ".txt", result);
        } catch (IOException e) {
            fail("Unexpected IO Exception: " + e.getMessage());
        }
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        CoreApplicationEnvironment.registerExtensionPoint(Extensions.getRootArea(), "com.intellij.lang.braceMatcher", LanguageExtensionPoint.class);
    }

    @Override
    protected Lexer createLexer() {
        return new HaskellSyntaxHighlightingLexer();
    }

    @Override
    protected String getDirPath() {
        return "tests" + File.separator + "gold";
    }

    /**
     * Loads the test data file from the right place.
     */
    protected String loadFile(@NonNls @TestDataFile String name) throws IOException {
        return doLoadFile(srcPath, name);
    }

    private static String doLoadFile(String myFullDataPath, String name) throws IOException {
        String text = FileUtil.loadFile(new File(myFullDataPath, name), CharsetToolkit.UTF8).trim();
        text = StringUtil.convertLineSeparators(text);
        return text;
    }

    /**
     * Check the result against a plain text file. Creates file if missing.
     * Avoids the default sandboxing in IntelliJ.
     */
    public static void doCheckResult(String fullPath, String targetDataName, String text) throws IOException {
        text = text.trim();
        String expectedFileName = fullPath + File.separator + targetDataName;
        if (OVERWRITE_TESTDATA) {
            VfsTestUtil.overwriteTestData(expectedFileName, text);
            System.out.println("File " + expectedFileName + " created.");
        }
        try {
            String expectedText = doLoadFile(fullPath, targetDataName);
            if (!Comparing.equal(expectedText, text)) {
                throw new FileComparisonFailure(targetDataName, expectedText, text, expectedFileName);
            }
        } catch(FileNotFoundException e){
            VfsTestUtil.overwriteTestData(expectedFileName, text);
            fail("No output text found. File " + expectedFileName + " created.");
        }
    }
}
