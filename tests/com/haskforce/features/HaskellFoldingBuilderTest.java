package com.haskforce.features;

import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase;
import com.intellij.openapi.editor.FoldRegion;

import java.io.File;

/**
 * Folding builder test driver. Add new folding builder testcases here.
 */
public class HaskellFoldingBuilderTest extends HaskellLightPlatformCodeInsightFixtureTestCase {
    public HaskellFoldingBuilderTest() {
        super("features", "features");
    }

    // The tests.

    // Fold00001 should really result in "  ". OPENCOM and CLOSECOM remains in
    // the text, but the rest gets collapsed to "  ".
    public void testFold00001() throws Throwable { doTest("  "); }
    public void testFold00002() throws Throwable { doTest("--"); }

    /**
     * Folds the region at the caret and verifies that it is indeed collapsed
     * and the placeholder text corresponds to the expected one.
     *
     * There is supposedly support in the base class for testing folding but
     * I had little luck when following the tutorial verbatim.
     */
    private void doTest(String collapsedText) {
        myFixture.configureByFile(getTestDataPath(getTestName(false) + ".hs"));
        myFixture.performEditorAction("CollapseAllRegions");
        FoldRegion[] all = myFixture.getEditor().getFoldingModel().getAllFoldRegions();
        for (FoldRegion region : all) {
            assertEquals(collapsedText, region.getPlaceholderText());
            assertFalse(region.isExpanded());
        }
    }
}
