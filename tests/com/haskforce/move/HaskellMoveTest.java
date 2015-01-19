package com.haskforce.move;

import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase;
import com.haskforce.psi.HaskellFile;
import com.intellij.psi.PsiFile;

public class HaskellMoveTest extends HaskellLightPlatformCodeInsightFixtureTestCase {

    public HaskellMoveTest() {
        super("move", "move");
    }

    public void testMove(){
        PsiFile[] files = myFixture.configureByFiles("From/MoveMe.hs", "To/Token.hs");
        myFixture.moveFile("From/MoveMe.hs", "To/");
        HaskellFile haskellFile = (HaskellFile)files[0];
        assertEquals("To",haskellFile.getContainingDirectory().getName());
        assertEquals("To.MoveMe",haskellFile.getModuleName());
    }
}
