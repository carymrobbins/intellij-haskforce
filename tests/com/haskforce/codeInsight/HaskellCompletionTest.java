package com.haskforce.codeInsight;

/**
 * Completion test driver. Add new completion testcases here.
 */
public class HaskellCompletionTest extends HaskellCompletionTestBase {
    public void testKeywords00001() throws Throwable {
        doTestInclude("-<caret>", "module", "import", "export");
    }
}
