package com.haskforce.highlighting.annotation.external;

import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase;
import com.haskforce.highlighting.annotation.HaskellAnnotationHolder;
import com.haskforce.highlighting.annotation.Problems;
import com.haskforce.psi.HaskellFile;
import com.haskforce.psi.impl.HaskellElementFactory;
import com.intellij.codeInsight.daemon.impl.AnnotationHolderImpl;
import com.intellij.lang.annotation.AnnotationHolder;
import com.intellij.lang.annotation.AnnotationSession;
import com.intellij.openapi.util.text.StringUtil;

import java.util.Arrays;
import java.util.Scanner;

/**
 * Tests for consuming output from ghc-mod.
 */
public class GhcModTest extends HaskellLightPlatformCodeInsightFixtureTestCase {
    public GhcModTest() {
        super("ghc-mod");
    }

    public void testParseProblems() {
        String stdout = StringUtil.join(Arrays.asList(
                "Warning: resolveModule \"/foo/bar/src/Main.hs\":",
                "         /foo/bar/src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)",
                "src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)"
        ), "\n");
        Scanner scanner = new Scanner(stdout);

        Problems problems = GhcMod.parseProblems(myModule, scanner);

        assertNotNull(problems);

        assertEquals(2, problems.size());

        GhcMod.Problem problem = (GhcMod.Problem)problems.get(0);
        assertNotNull(problem);
        assertEquals(5, problem.startLine);
        assertEquals(1, problem.startColumn);
        assertEquals("/foo/bar/src/Main.hs", problem.file);
        assertEquals("parse error (possibly incorrect indentation or mismatched brackets)", problem.message);

        problem = (GhcMod.Problem)problems.get(1);
        assertNotNull(problem);
        assertEquals(5, problem.startLine);
        assertEquals(1, problem.startColumn);
        assertEquals("src/Main.hs", problem.file);
        assertEquals("parse error (possibly incorrect indentation or mismatched brackets)", problem.message);
    }

    public void testDuplicateAnnotations() {
        HaskellFile file = HaskellElementFactory.createFileFromText(getProject(),
                StringUtil.join(Arrays.asList(
                        "module Main where",
                        "",
                        "import ",
                        "",
                        "main ="
                ), "\n")
        );
        file.setName("src/Main.hs");

        // Intentionally leaving both paths to /src/Main.hs the same to ensure that
        // createAnnotations sees both as the same file and handles the duplicate properly.
        String stdout = StringUtil.join(Arrays.asList(
                "Warning: resolveModule \"/foo/bar/src/Main.hs\":",
                "         /src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)",
                "/src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)"
        ), "\n");
        Scanner scanner = new Scanner(stdout);

        Problems problems = GhcMod.parseProblems(myModule, scanner);

        HaskellAnnotationHolder holder = new HaskellAnnotationHolder(
                new AnnotationHolderImpl(new AnnotationSession(file))
        );

        HaskellExternalAnnotator.createAnnotations(file, problems, holder);

        assertEquals(1, ((AnnotationHolderImpl)holder.holder).size());
    }
}
