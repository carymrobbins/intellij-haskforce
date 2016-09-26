package com.haskforce.haskell.codeInsight;

import com.haskforce.haskell.HaskellLightPlatformCodeInsightFixtureTestCase;
import com.haskforce.haskell.psi.HaskellConid;
import com.haskforce.haskell.psi.HaskellVarid;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;



public class HaskellGoToSymbolTest extends HaskellLightPlatformCodeInsightFixtureTestCase {
    public HaskellGoToSymbolTest() {
        super("codeInsight", "codeInsight");
    }

    public void testGoToSymbolFunction_SymbolOnUsage(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        String textOfFile = file.getText();
        int expectedStartOffset= textOfFile.indexOf("let seven") +4;
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid)reference.resolve();
        assertEquals(varId.getName(), referencedElement.getName());
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testNewtypeParameter(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        String textOfFile = file.getText();
        int expectedStartOffset= textOfFile.indexOf("ResultVar param") +10;
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid)reference.resolve();
        assertEquals(varId.getName(), referencedElement.getName());
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_GADT(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        String textOfFile = file.getText();
        int expectedStartOffset= textOfFile.indexOf("f (B _ end)") +7;
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid)reference.resolve();
        assertEquals(varId.getName(), referencedElement.getName());
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }


    public void testGoToSymbolFunction_Pattern_CaretOnVariable(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        String textOfFile = file.getText();
        int expectedStartOffset= textOfFile.indexOf("let (seven") +5;
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid)reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset,referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_Monads_CaretOnVariable(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        String textOfFile = file.getText();
        int expectedStartOffset= textOfFile.indexOf("seven <-");
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid)reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset,referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_BehaveWhenCaretOutsideAFunction(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        PsiReference reference = psiElement.getReference();
        assertNull(reference);
    }

    public void testGoToSymbolFunction_CaretOnFunctionInstance(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        String textOfFile = file.getText();
        int expectedStartOffset= textOfFile.indexOf("test ::");
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid)reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_CaretOnFunctionDeclaration(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        String textOfFile = file.getText();
        int expectedStartOffset= textOfFile.indexOf("test ::");
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid)reference.resolve();
        assertSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset,referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_CanReferenceOtherFunction(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        String textOfFile = file.getText();
        int expectedStartOffset= textOfFile.indexOf("test2 ::");
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid)reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset,referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_ReferenceLeftMostFunctionWithoutTypeSig(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        String textOfFile = file.getText();
        int expectedStartOffset= textOfFile.indexOf("test 1 =");
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid)reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset,referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_WhereClause(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        String textOfFile = file.getText();
        int expectedStartOffset= textOfFile.indexOf("where seven") + 6;
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid)reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset,referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolModuleDoesntResolveToDataConstructor(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellConid haskellConid = (HaskellConid) psiElement;
        PsiReference reference = haskellConid.getReference();
        HaskellConid referencedElement = (HaskellConid)reference.resolve();
        assertNull(referencedElement);
    }

    public void testGoToSymbolFunction_Records(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        String textOfFile = file.getText();
        int expectedStartOffset= textOfFile.indexOf("Pool maybeA") + 5;
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid)reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset,referencedElement.getTextRange().getStartOffset());
    }

    /**
     * TODO
     * Can only make this test work if findDefinitionNode returns the type declaration as well
     */
    public void ignoreTestGoToSymbolFunction_RecordsType(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        String textOfFile = file.getText();
        int expectedStartOffset= textOfFile.indexOf("data Pool") + 5;
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellConid conId = (HaskellConid) psiElement;
        PsiReference reference = conId.getReference();
        HaskellConid referencedElement = (HaskellConid)reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset,referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_RecordsConstructor(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        String textOfFile = file.getText();
        int expectedStartOffset= textOfFile.indexOf("Pool (Maybe a)");
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellConid conId = (HaskellConid) psiElement;
        PsiReference reference = conId.getReference();
        HaskellConid referencedElement = (HaskellConid)reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset,referencedElement.getTextRange().getStartOffset());
    }



    /**
     * TODO : try to get these tests to follow the same approach as most others, with getTestName instead of lots of literal stuff
     */

    public void testGoToSymbolFunction_TotalProject(){
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "TotalProject/DungeonMaster/Fight.hs",
                "TotalProject/Game/GameState.hs",
                "TotalProject/Board/Adventure/Adventure.hs",
                "TotalProject/Board/Follower/Follower.hs",
                "TotalProject/Board/Object/Object.hs",
                "TotalProject/Board/Space/Space.hs",
                "TotalProject/Character/Character.hs",
                "TotalProject/DungeonMaster/DungeonMaster.hs",
                "TotalProject/DungeonMaster/Movement.hs");
        PsiFile dungeonMasterFight = psiFiles[0];
        PsiFile gameGamestate = psiFiles[1];
        String textOfFile = gameGamestate.getText();
        int expectedStartOffset= textOfFile.indexOf("updatePlayer ::");
        PsiElement psiElement = dungeonMasterFight
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid)reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset,referencedElement.getTextRange().getStartOffset());
        /**
         * TODO maybe also compare the containg files of both elements to be the same. To be sure.
         * But likely a little bit overkill. Nevertheless.
         */
    }

    public void testGoToSymbolFunction_QualifiedImport() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "QualifiedImport/Usage.hs",
                "QualifiedImport/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        PsiFile definition = psiFiles[1];
        String textOfFile = definition.getText();
        int expectedStartOffset = textOfFile.indexOf("seven ::");
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_QualifiedImportNoAs() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "QualifiedImportNoAs/Usage.hs",
                "QualifiedImportNoAs/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        PsiFile definition = psiFiles[1];
        String textOfFile = definition.getText();
        int expectedStartOffset = textOfFile.indexOf("seven ::");
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_QualifiedImportMultipleLevels() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "QualifiedImportMultipleLevels/Usage/Usage.hs",
                "QualifiedImportMultipleLevels/Definition/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        PsiFile definition = psiFiles[1];
        String textOfFile = definition.getText();
        int expectedStartOffset = textOfFile.indexOf("seven ::");
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_QualifiedImportMultipleLevels_LocalFunctionWithSameName() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "QualifiedImportMultipleLevels_LocalFunctionWithSameName/Usage/Usage.hs",
                "QualifiedImportMultipleLevels_LocalFunctionWithSameName/Definition/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        PsiFile definition = psiFiles[1];
        String textOfFile = definition.getText();
        int expectedStartOffset = textOfFile.indexOf("seven ::");
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_QualifiedImportMultipleLevels_LocalFunctionWithSameName_CaretOnLocal() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "QualifiedImportMultipleLevels_LocalFunctionWithSameName_CaretOnLocal/Usage/Usage.hs",
                "QualifiedImportMultipleLevels_LocalFunctionWithSameName_CaretOnLocal/Definition/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        String textOfFile = usage.getText();
        int expectedStartOffset = textOfFile.indexOf("seven ::");
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_QualifiedImportMultipleLevels_AsPartConsistsOfMultipleCons() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "QualifiedImportMultipleLevels_AsPartConsistsOfMultipleCons/Usage/Usage.hs",
                "QualifiedImportMultipleLevels_AsPartConsistsOfMultipleCons/Definition/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        PsiFile definition = psiFiles[1];
        String textOfFile = definition.getText();
        int expectedStartOffset = textOfFile.indexOf("seven ::");
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }


    public void testGoToSymbolFunction_ImportResolvesToModule() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "ImportResolvesToModule/Usage.hs",
                "ImportResolvesToModule/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        PsiFile definition = psiFiles[1];
        String textOfFile = definition.getText();
        int expectedStartOffset = textOfFile.indexOf("module Definition") + 7;
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellConid conId = (HaskellConid) psiElement;
        PsiReference reference = conId.getReference();
        HaskellConid referencedElement = (HaskellConid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_ImportResolvesToModule_MultipleCons_OnModule() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "ImportResolvesToModule_MultipleCons_OnModule/Usage.hs",
                "ImportResolvesToModule_MultipleCons_OnModule/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        PsiFile definition = psiFiles[1];
        String textOfFile = definition.getText();
        int expectedStartOffset = textOfFile.indexOf("module Definition.Definition") + 18;
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellConid conId = (HaskellConid) psiElement;
        PsiReference reference = conId.getReference();
        HaskellConid referencedElement = (HaskellConid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_ImportResolvesToModule_MultipleCons_OnPrefix() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "ImportResolvesToModule_MultipleCons_OnPrefix/Usage.hs",
                "ImportResolvesToModule_MultipleCons_OnPrefix/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        PsiFile definition = psiFiles[1];
        String textOfFile = definition.getText();
        int expectedStartOffset = textOfFile.indexOf("module Definition.Definition") + 7;
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellConid conId = (HaskellConid) psiElement;
        PsiReference reference = conId.getReference();
        HaskellConid referencedElement = (HaskellConid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }


    public void testGoToSymbolFunction_QualifiedImportQualifierresolves() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "QualifiedImport_QualifierResolves/Usage.hs",
                "QualifiedImport_QualifierResolves/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        String textOfFile = usage.getText();
        int expectedStartOffset = textOfFile.indexOf("as Def") + 3;
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellConid conId = (HaskellConid) psiElement;
        PsiReference reference = conId.getReference();
        HaskellConid referencedElement = (HaskellConid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_QualifiedImportQualifierresolvesMultipleCons_Cons1() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "QualifiedImport_QualifierResolvesMultipleCons_Cons1/Usage.hs",
                "QualifiedImport_QualifierResolvesMultipleCons_Cons1/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        String textOfFile = usage.getText();
        int expectedStartOffset = textOfFile.indexOf("as Def.Lef") + 3;
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellConid conId = (HaskellConid) psiElement;
        PsiReference reference = conId.getReference();
        HaskellConid referencedElement = (HaskellConid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_QualifiedImportQualifierresolvesMultipleCons_Cons2() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "QualifiedImport_QualifierResolvesMultipleCons_Cons2/Usage.hs",
                "QualifiedImport_QualifierResolvesMultipleCons_Cons2/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        String textOfFile = usage.getText();
        int expectedStartOffset = textOfFile.indexOf("as Def.Lef") + 7;
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellConid conId = (HaskellConid) psiElement;
        PsiReference reference = conId.getReference();
        HaskellConid referencedElement = (HaskellConid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_QualifiedImportQualifierresolvesMultipleCons_Cons1_NoAs() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "QualifiedImport_QualifierResolvesMultipleCons_Cons1_NoAs/Usage.hs",
                "QualifiedImport_QualifierResolvesMultipleCons_Cons1_NoAs/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        String textOfFile = usage.getText();
        int expectedStartOffset = textOfFile.indexOf("qualified Definition.Definition") + 10;
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellConid conId = (HaskellConid) psiElement;
        PsiReference reference = conId.getReference();
        HaskellConid referencedElement = (HaskellConid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_QualifiedImportQualifierresolvesMultipleCons_Cons2_NoAs() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "QualifiedImport_QualifierResolvesMultipleCons_Cons2_NoAs/Usage.hs",
                "QualifiedImport_QualifierResolvesMultipleCons_Cons2_NoAs/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        String textOfFile = usage.getText();
        int expectedStartOffset = textOfFile.indexOf("qualified Definition.Definition") + 21;
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellConid conId = (HaskellConid) psiElement;
        PsiReference reference = conId.getReference();
        HaskellConid referencedElement = (HaskellConid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_QualifiedImport_MultipleImportSameQualifiedName1() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "QualifiedImport_MultipleImportSameQualifiedName1/Usage/Usage.hs",
                "QualifiedImport_MultipleImportSameQualifiedName1/Definition/Definition.hs",
                "QualifiedImport_MultipleImportSameQualifiedName1/Other/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        String textOfFile = usage.getText();
        int expectedStartOffset = textOfFile.indexOf("qualified Definition.Definition as Definition") + 35;
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellConid conId = (HaskellConid) psiElement;
        PsiReference reference = conId.getReference();
        HaskellConid referencedElement = (HaskellConid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }

    public void testGoToSymbolFunction_QualifiedImport_MultipleImportSameQualifiedName2() {
        PsiFile[] psiFiles = myFixture.configureByFiles(
                "QualifiedImport_MultipleImportSameQualifiedName2/Usage/Usage.hs",
                "QualifiedImport_MultipleImportSameQualifiedName2/Definition/Definition.hs",
                "QualifiedImport_MultipleImportSameQualifiedName2/Other/Definition.hs"
        );
        PsiFile usage = psiFiles[0];
        String textOfFile = usage.getText();
        int expectedStartOffset = textOfFile.indexOf("qualified Other.Definition as Definition") + 30;
        PsiElement psiElement = usage
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellConid conId = (HaskellConid) psiElement;
        PsiReference reference = conId.getReference();
        HaskellConid referencedElement = (HaskellConid) reference.resolve();
        assertNotSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }




}
