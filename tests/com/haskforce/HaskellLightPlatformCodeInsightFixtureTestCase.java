package com.haskforce;

/*
 * Downloaded and adapted from the Erlang Plugin on 12 May 2014.
 */

import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.projectRoots.ProjectJdkTable;
import com.intellij.openapi.projectRoots.Sdk;
import com.intellij.openapi.roots.ProjectRootManager;
import com.intellij.openapi.util.io.FileUtil;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.openapi.vfs.CharsetToolkit;
import com.intellij.testFramework.TestDataFile;
import com.intellij.testFramework.fixtures.LightPlatformCodeInsightFixtureTestCase;
import org.jetbrains.annotations.NonNls;

import java.io.File;
import java.io.IOException;

/**
 * Lightweight test case base.  Note that file paths must use '/' instead of `File.separatorChar`.
 */
public abstract class HaskellLightPlatformCodeInsightFixtureTestCase extends LightPlatformCodeInsightFixtureTestCase {
    private String srcPath;
    private String expectPath;

    /**
     * Sets the expected input and outputs and calls the constructor of the parent.
     * @param srcName Directory name of test inputs.
     * @param expectName Directory name of expected outputs.
     */
    protected HaskellLightPlatformCodeInsightFixtureTestCase(String srcName, String expectName) {
        super();
        srcPath = getDirPath() + '/' + srcName;
        expectPath = getDirPath() + '/' + expectName;
    }

    protected HaskellLightPlatformCodeInsightFixtureTestCase(String name) {
        this(name, name);
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }

    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
    }

    @Override
    protected String getTestDataPath() {
        return srcPath;
    }

    protected String getTestDataPath(String... names) {
        return srcPath + '/' + StringUtil.join(names, "/");
    }

    /**
     * Base path to the test files.
     */
    protected static String getDirPath() {
        return "tests/gold";
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

    protected void setUpProjectSdk() {
        ApplicationManager.getApplication().runWriteAction(new Runnable() {
            @Override
            public void run() {
                Sdk sdk = getProjectDescriptor().getSdk();
                ProjectJdkTable.getInstance().addJdk(sdk);
                ProjectRootManager.getInstance(myFixture.getProject()).setProjectSdk(sdk);
            }
        });
    }
}
