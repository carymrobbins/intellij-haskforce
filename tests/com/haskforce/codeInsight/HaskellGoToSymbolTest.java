package com.haskforce.codeInsight;

import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase;
import com.haskforce.psi.HaskellVarid;
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

    public void testGoToSymbolFunction_SymbolOnDeclaration(){
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
        assertEquals(expectedStartOffset,referencedElement.getTextRange().getStartOffset());
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
}
