package com.haskforce.features.intentions;

import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase;


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
    }
}