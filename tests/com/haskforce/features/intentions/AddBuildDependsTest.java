package com.haskforce.features.intentions;

import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase;
import com.haskforce.cabal.psi.CabalDependency;
import com.haskforce.cabal.psi.CabalFile;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.util.PsiTreeUtil;

import java.util.Collection;
import java.util.Iterator;


//Maybe rename the haskelllightplatformcodeinsightfixturetestclass, as it actually has no
//real link with haskell tests, but can also be used for cabal tests
/**
TODO still fails, I have to see how I can manage to run a test with a 'UI' part (
 it will show a pop up and require a selection in that pop up, don't know how
 to mock this out). An option might be FEST (https://code.google.com/p/fest/) to
 test the swing part, but I suspect that more will be necessary as right now this
 test will fail as it tries to show a pop up while there is no screen to be found
 **/
public class AddBuildDependsTest extends HaskellLightPlatformCodeInsightFixtureTestCase {
    public AddBuildDependsTest(){
        super("features/intentions", "features/intentions");
    }

    public void testAddBuildDependsInvoke() throws Exception {
        PsiFile[] psiFiles = myFixture.configureByFiles(getTestName(false)+".cabal");
        CabalFile cabalFile = (CabalFile)psiFiles[0];
        AddBuildDepends dingske = new AddBuildDepends("dingske");
        dingske.invoke(myFixture.getProject(),myFixture.getEditor(),cabalFile);
        Collection<CabalDependency> cabalDependencies = PsiTreeUtil.findChildrenOfType(cabalFile, CabalDependency.class);
        assertEquals(2, cabalDependencies.size());
        Iterator<CabalDependency> cabalDependencyIterator = cabalDependencies.iterator();
        assertEquals ("dongske", cabalDependencyIterator.next().getDependencyName().getText());
        assertEquals ("dingske", cabalDependencyIterator.next().getDependencyName().getText());
    }

}