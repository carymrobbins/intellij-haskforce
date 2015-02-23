package com.haskforce.features.intentions;

import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase;
import com.haskforce.cabal.psi.CabalDependency;
import com.haskforce.cabal.psi.CabalFile;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;


//Maybe rename the haskelllightplatformcodeinsightfixturetestclass, as it actually has no
//real link with haskell tests, but can also be used for cabal tests
public class AddBuildDependsTest extends HaskellLightPlatformCodeInsightFixtureTestCase {
    public AddBuildDependsTest(){
        super("features/intentions", "features/intentions");
    }

    public void testInvoke() throws Exception {
        myFixture.configureByFiles("addbuilddepends.cabal");
        AddBuildDepends dingske = new AddBuildDepends("dingske");
        dingske.invoke(myFixture.getProject(),myFixture.getEditor(),null);
        CabalFile cabalFile = (CabalFile)myFixture.getFile();
        CabalDependency[] childrenOfType = PsiTreeUtil.getChildrenOfType(cabalFile, CabalDependency.class);
        assertEquals(2, childrenOfType.length);

    }
}