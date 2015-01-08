package com.haskforce.codeInsight;

import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase;
import com.haskforce.psi.HaskellVarid;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiReference;
import jdk.nashorn.internal.ir.annotations.Ignore;

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

    //Might not be necessary
    /*public void testGoToSymbolFunction_SymbolOnDeclaration(){
        myFixture.configureByFile(getTestName(false)+".hs");
        PsiFile file = myFixture.getFile();
        String textOfFile = file.getText();
        int expectedStartOffset= textOfFile.indexOf("let seven") +4;
        PsiElement psiElement = file
                .findElementAt(myFixture.getCaretOffset()).getParent();
        HaskellVarid varId = (HaskellVarid) psiElement;
        PsiReference reference = varId.getReference();
        HaskellVarid referencedElement = (HaskellVarid)reference.resolve();
        assertSame(psiElement, referencedElement);
        assertEquals(expectedStartOffset, referencedElement.getTextRange().getStartOffset());
    }*/

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
}
