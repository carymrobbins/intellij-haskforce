package com.haskforce.highlighting.annotation.external

import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase
import com.haskforce.highlighting.annotation.HaskellAnnotationHolder
import com.haskforce.psi.impl.HaskellElementFactory
import com.haskforce.test.AssertMixin
import com.intellij.codeInsight.daemon.impl.AnnotationHolderImpl
import com.intellij.lang.annotation.{AnnotationHolder, AnnotationSession}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.util.text.StringUtil

import java.util
import scala.collection.JavaConverters._

/**
 * Tests for consuming output from ghc-mod.
 */
class GhcModTest extends HaskellLightPlatformCodeInsightFixtureTestCase("ghc-mod") with AssertMixin {

    def testParseProblems(): Unit = {
        val stdout = StringUtil.join(util.Arrays.asList(
                "Warning: resolveModule \"/foo/bar/src/Main.hs\":",
                "         /foo/bar/src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)",
                "src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)"
        ), "\n")
        val scanner = new util.Scanner(stdout)

        val problems = GhcMod.parseProblems(myFixture.getModule, scanner)

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
        ApplicationManager.getApplication.runWriteAction({ () =>
          file.setName("src/Main.hs")
          ()
        }: Runnable)

        // Intentionally leaving both paths to /src/Main.hs the same to ensure that
        // createAnnotations sees both as the same file and handles the duplicate properly.
        val stdout = StringUtil.join(util.Arrays.asList(
          "Warning: resolveModule \"/foo/bar/src/Main.hs\":",
          "         /src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)",
          "/src/Main.hs:5:1:parse error (possibly incorrect indentation or mismatched brackets)"
        ), "\n")
        val scanner = new util.Scanner(stdout)

        val problems = GhcMod.parseProblems(myFixture.getModule, scanner).asScala.toStream

        val holder = new HaskellAnnotationHolder(
          newAnnotationHolderImpl(new AnnotationSession(file))
        )

        HaskellExternalAnnotator.createAnnotations(file, problems, holder)

        assertEquals(1, holder.holder.asInstanceOf[AnnotationHolderImpl].size())
    }

    // Temporarily bypass that AnnotationHolderImpl is deprecated.
    // TODO: It would be better to test like this: https://plugins.jetbrains.com/docs/intellij/annotator-test.html#define-a-test-method
    private def newAnnotationHolderImpl(session: AnnotationSession): AnnotationHolder = {
      getClass.getClassLoader.loadClass("com.intellij.codeInsight.daemon.impl.AnnotationHolderImpl")
        .getConstructor(classOf[AnnotationSession])
        .newInstance(session)
        .asInstanceOf[AnnotationHolder]
    }
}
