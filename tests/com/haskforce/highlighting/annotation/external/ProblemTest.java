package com.haskforce.highlighting.annotation.external;

import com.haskforce.features.intentions.AddBuildDepends;
import com.haskforce.features.intentions.AddLanguagePragma;
import com.haskforce.features.intentions.RemoveForall;
import com.intellij.lang.annotation.Annotation;
import com.intellij.lang.annotation.HighlightSeverity;
import junit.framework.TestCase;

import java.util.List;

public class ProblemTest extends TestCase {

    public void testRegisterIllegalSymbolFix() throws Exception {
        GhcMod.Problem illegalSymbolProblem
                = new GhcMod.Problem("file.42", 0, 0, "Illegal symbol '.' in type");
        Annotation dummyAnnotation = new Annotation(0,0, HighlightSeverity.ERROR,"","");

        illegalSymbolProblem.registerFix(dummyAnnotation);

        List<Annotation.QuickFixInfo> quickFixes = dummyAnnotation.getQuickFixes();
        assertEquals(2, quickFixes.size());
        assertTrue(quickFixes.get(0).quickFix instanceof AddLanguagePragma);
        AddLanguagePragma rankNTypesPragma =(AddLanguagePragma)quickFixes.get(0).quickFix;
        assertEquals("RankNTypes", rankNTypesPragma.languageName);
        assertTrue(quickFixes.get(1).quickFix instanceof RemoveForall);
    }

    public void testRegisterScopedTypeVariablesFix() throws Exception {
        GhcMod.Problem illegalSymbolProblem
                = new GhcMod.Problem("file.42", 0, 0, "Illegal type signature: Maybe Int\n" +
                "      Perhaps you intended to use ScopedTypeVariables\n" +
                "    In a pattern type-signature");
        Annotation dummyAnnotation = new Annotation(0,0, HighlightSeverity.ERROR,"","");

        illegalSymbolProblem.registerFix(dummyAnnotation);

        List<Annotation.QuickFixInfo> quickFixes = dummyAnnotation.getQuickFixes();
        assertNotNull(quickFixes);
        assertEquals(1, quickFixes.size());
        assertTrue(quickFixes.get(0).quickFix instanceof AddLanguagePragma);
        AddLanguagePragma rankNTypesPragma =(AddLanguagePragma)quickFixes.get(0).quickFix;
        assertEquals("ScopedTypeVariables", rankNTypesPragma.languageName);
    }

    public void testAddTastyBuildDependsFix() throws Exception {
        GhcMod.Problem illegalSymbolProblem
                = new GhcMod.Problem("file.42", 0, 0, "Could not find module ‘Test.Tasty’\n" +
                "    It is a member of the hidden package ‘tasty-0.10.1’.\n" +
                "    Perhaps you need to add ‘tasty’ to the build-depends in your .cabal file.");
        Annotation dummyAnnotation = new Annotation(0,0, HighlightSeverity.ERROR,"","");

        illegalSymbolProblem.registerFix(dummyAnnotation);

        List<Annotation.QuickFixInfo> quickFixes = dummyAnnotation.getQuickFixes();
        assertNotNull(quickFixes);
        assertEquals(1, quickFixes.size());
        assertTrue(quickFixes.get(0).quickFix instanceof AddBuildDepends);
        AddBuildDepends buildDependsTasty =(AddBuildDepends)quickFixes.get(0).quickFix;
        assertEquals("tasty", buildDependsTasty.packageName);
    }
}