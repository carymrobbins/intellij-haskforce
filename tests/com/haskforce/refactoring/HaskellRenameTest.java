/*
 * Copyright 2012-2014 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.haskforce.refactoring;

import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.psi.PsiFile;

/**
 * Rename refactoring test driver. Add new rename testcases here.
 */
public class HaskellRenameTest extends HaskellLightPlatformCodeInsightFixtureTestCase {
    public HaskellRenameTest() {
        super("refactoring", "refactoring");
    }

    public void testFunction()    { doTest("bar"); }
    public void testFunctionInstance()    { doTest("bar"); }
    public void testLocalVariable()    { doTest("bars"); }
    public void testRenameModule()    { doTest("BarModule", true); }

    public void testRenameImport () {
        PsiFile[] psiFiles = myFixture.configureByFiles("ImportDeclaration/ImportDeclaration.hs", "ImportDeclaration/RenameImport.hs");
        PsiFile importDeclaration = psiFiles[0];
        String newName = "BarModule";
        myFixture.renameElementAtCaret(newName);
        myFixture.checkResultByFile("ImportDeclaration/BarModule.hs","ImportDeclaration/ImportDeclaration-after.hs", false);
        myFixture.checkResultByFile("ImportDeclaration/RenameImport.hs","ImportDeclaration/RenameImport-after.hs", false);
        assertEquals(StringUtil.unquoteString(newName) + ".hs", importDeclaration.getName());

    }

    public void testRenameImport_CaretOnModule () {
        PsiFile[] psiFiles = myFixture.configureByFiles("ImportDeclaration_CaretOnModule/ImportDeclaration.hs", "ImportDeclaration_CaretOnModule/RenameImport.hs");
        PsiFile importDeclaration = psiFiles[0];
        String newName = "BarModule";
        myFixture.renameElementAtCaret(newName);
        myFixture.checkResultByFile("ImportDeclaration_CaretOnModule/BarModule.hs", "ImportDeclaration_CaretOnModule/ImportDeclaration-after.hs", false);
        myFixture.checkResultByFile("ImportDeclaration_CaretOnModule/RenameImport.hs","ImportDeclaration_CaretOnModule/RenameImport-after.hs", false);
        assertEquals(StringUtil.unquoteString(newName) + ".hs", importDeclaration.getName());
    }

    public void testRenameFile() {
        PsiFile[] psiFiles = myFixture.configureByFiles("ImportDeclaration_CaretOnModule/ImportDeclaration.hs", "ImportDeclaration_CaretOnModule/RenameImport.hs");
        PsiFile importDeclaration = psiFiles[0];
        String newName = "BarModule";
        myFixture.renameElement(importDeclaration,newName);
        myFixture.checkResultByFile("ImportDeclaration_CaretOnModule/BarModule.hs", "ImportDeclaration_CaretOnModule/ImportDeclaration-after.hs", false);
        myFixture.checkResultByFile("ImportDeclaration_CaretOnModule/RenameImport.hs","ImportDeclaration_CaretOnModule/RenameImport-after.hs", false);
        assertEquals(StringUtil.unquoteString(newName) + ".hs", importDeclaration.getName());
    }


    private void doTest(String newName) {
        doTest(newName, false);
    }

    private void doTest(String newName, boolean isRenameModuleTest) {
        myFixture.testRename(getTestName(false) + ".hs", getTestName(false) + "-after.hs", newName);
        if (isRenameModuleTest) {
            assertEquals(StringUtil.unquoteString(newName) + ".hs", myFixture.getFile().getName());
        }
    }

}
