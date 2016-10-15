package com.haskforce.tools.cabal.completion

import java.io.File
import java.util
import java.util.regex.Pattern

import scala.collection.JavaConverters._

import com.intellij.codeInsight.completion.CompletionType
import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.util.text.StringUtil
import com.intellij.openapi.vfs.CharsetToolkit
import com.intellij.psi.PsiFile

import com.haskforce.tools.cabal.CabalFileType
import com.haskforce.haskell.codeInsight.HaskellCompletionTestBase
import com.haskforce.haskell.codeInsight.HaskellCompletionTestBase.CheckType
import com.haskforce.macros.string.dedent

class CabalCompletionTest extends CabalCompletionTestBase {

  def testModule00001() = doInclude("Parser")

  /** Complete only the extensions not already provided. */
  def testExtensions00001() = doTestEqual(
    dedent("""
      library
        default-extensions:
          NoMonoLocalBinds NoMono<caret>
    """),
    "NoMonomorphismRestriction", "NoMonoPatBinds"
  )

  /** Don't complete negated equivalent of extensions already provided. */
  def testExtensions00002() = doTestExclude(
    dedent("""
      library
        other-extensions:
          CPP
    """),
    "CPP", "NoCPP"
  )
}

class CabalCompletionTestBase
  extends HaskellCompletionTestBase(FileUtil.join("cabal", "completion")) {

  override protected def getFileType = CabalFileType.INSTANCE

  protected def doInclude(expect1: String, expectN: String*): Unit = {
    fileWithCaret match {
      case Some(f) =>
        myFixture.configureFromExistingVirtualFile(f.getVirtualFile)
        doTestVariantsInner(
          CompletionType.BASIC,
          /* invocationCount */ 1,
          CheckType.INCLUDES,
          expect1 +: expectN: _*
        )
      case None =>
        throw new AssertionError("Expected at least one file with <caret> but found none")
    }
  }

  override protected def getTestDataPath(): String = {
    FileUtil.join(super.getTestDataPath, getTestName(false))
  }

  private def getTestDataFiles(): util.Collection[File] = {
    com.haskforce.system.utils.FileUtil.findFilesRecursively(new File(getTestDataPath()))
  }

  var fileWithCaret: Option[PsiFile] = None
  var caretOffset: Option[Int] = None

  override protected def setUp(): Unit = {
    super.setUp()
    getTestDataFiles().iterator().asScala.foreach { file =>
      if (!file.isDirectory) {
        val text = StringUtil.convertLineSeparators(
          FileUtil.loadFile(file, CharsetToolkit.UTF8)
        )
        val psiFile = myFixture.configureByText(file.getName, text)
        val pos = text.indexOf("<caret>")
        if (pos != -1) {
          fileWithCaret.foreach { f =>
            throw new AssertionError(
              "Expected only one file to have <caret>, but found two:\n" +
                file.getCanonicalPath + "\n" +
                "and " + f.getVirtualFile.getCanonicalPath
            )
          }
          fileWithCaret = Some(psiFile)
          caretOffset = Some(pos)
        }
      }
    }
  }
}
