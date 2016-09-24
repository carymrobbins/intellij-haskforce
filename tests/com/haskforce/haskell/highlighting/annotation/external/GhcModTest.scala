package com.haskforce.haskell.highlighting.annotation.external

import java.util

import com.haskforce.haskell.HaskellLightPlatformCodeInsightFixtureTestCase

import scala.collection.JavaConverters._
import junit.framework.TestCase
import com.haskforce.haskell.psi.impl.HaskellElementFactory
import com.haskforce.system.integrations.highlighting.HaskellAnnotationHolder
import com.intellij.codeInsight.daemon.impl.AnnotationHolderImpl
import com.intellij.lang.annotation.AnnotationSession
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.util.text.StringUtil

/**
 * Tests for consuming output from ghc-mod.
 */
class GhcModTest extends HaskellLightPlatformCodeInsightFixtureTestCase("ghc-mod") {
    import TestCase._

    def testParseProblems(): Unit = {
        val stdout = StringUtil.join(util.Arrays.asList(
                "Warning: resolveModule \"/foo/bar/src/Main.hs\":",
                "         /foo/bar/src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)",
                "src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)"
        ), "\n")
        val scanner = new util.Scanner(stdout)

        val problems = GhcMod.parseProblems(myModule, scanner)

        assertNotNull(problems)

        assertEquals(2, problems.size())

        var problem = problems.get(0).asInstanceOf[GhcMod.Problem]
        assertNotNull(problem)
        assertEquals(5, problem.startLine)
        assertEquals(1, problem.startColumn)
        assertEquals("/foo/bar/src/Main.hs", problem.file)
        assertEquals("parse error (possibly incorrect indentation or mismatched brackets)", problem.message)

        problem = problems.get(1).asInstanceOf[GhcMod.Problem]
        assertNotNull(problem)
        assertEquals(5, problem.startLine)
        assertEquals(1, problem.startColumn)
        assertEquals("src/Main.hs", problem.file)
        assertEquals("parse error (possibly incorrect indentation or mismatched brackets)", problem.message)
    }

    def testDuplicateAnnotations(): Unit = {
        val file = HaskellElementFactory.createFileFromText(getProject,
          StringUtil.join(util.Arrays.asList(
                  "module Main where",
                  "",
                  "import ",
                  "",
                  "main ="
          ), "\n")
        )
        ApplicationManager.getApplication.runWriteAction(new Runnable() {
          def run() = file.setName("src/Main.hs")
        })

        // Intentionally leaving both paths to /src/Main.hs the same to ensure that
        // createAnnotations sees both as the same file and handles the duplicate properly.
        val stdout = StringUtil.join(util.Arrays.asList(
          "Warning: resolveModule \"/foo/bar/src/Main.hs\":",
          "         /src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)",
          "/src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)"
        ), "\n")
        val scanner = new util.Scanner(stdout)

        val problems = GhcMod.parseProblems(myModule, scanner).asScala.toStream

        val holder = new HaskellAnnotationHolder(
          new AnnotationHolderImpl(new AnnotationSession(file))
        )

        HaskellExternalAnnotator.createAnnotations(file, problems, holder)

        assertEquals(1, holder.holder.asInstanceOf[AnnotationHolderImpl].size())
    }
}
