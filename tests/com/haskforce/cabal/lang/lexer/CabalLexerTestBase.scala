package com.haskforce.cabal.lang.lexer

import java.io.{File, FileNotFoundException, IOException}
import java.util.Objects

import com.intellij.lang.TokenWrapper
import com.intellij.lexer.Lexer
import com.intellij.openapi.util.io.FileUtil
import com.intellij.openapi.util.text.StringUtil
import com.intellij.openapi.vfs.CharsetToolkit
import com.intellij.rt.execution.junit.FileComparisonFailure
import com.intellij.testFramework.{LexerTestCase, TestDataFile, UsefulTestCase, VfsTestUtil}
import junit.framework.TestCase
import org.jetbrains.annotations.NonNls

abstract class CabalLexerTestBase extends LexerTestCase {

  val OVERWRITE_TESTDATA = UsefulTestCase.OVERWRITE_TESTDATA

  protected def getExpectedPath: String

  override lazy val getDirPath: String = {
    FileUtil.join("tests", "gold", "cabal", "parser")
  }

  def doTest(): Unit = {
    val testName = getTestName(/* lowercaseFirstLetter = */ true)
    val fileName = testName + ".cabal"
    val text = loadFile(fileName)
    checkSegments(text, createLexer())
    val result = printTokensExtra(text, 0)
    val expectedPath = FileUtil.join(getDirPath, getExpectedPath, "expected")
    doCheckResult(expectedPath, testName + ".txt", result)
  }

  @throws[IOException]
  protected def loadFile(@NonNls @TestDataFile name: String): String  = {
    doLoadFile(getDirPath, name)
  }


  @throws[IOException]
  private def doLoadFile(myFullDataPath: String, name: String): String  = {
    StringUtil.convertLineSeparators(
      FileUtil.loadFile(new File(myFullDataPath, name), CharsetToolkit.UTF8).trim
    )
  }

  /**
   * Check the result against a plain text file. Creates file if missing.
   * Avoids the default sandboxing in IntelliJ.
   */
  @throws[IOException]
  def doCheckResult(fullPath: String, targetDataName: String, _text: String): Unit = {
    val text = _text.trim
    val expectedFileName = FileUtil.join(fullPath, targetDataName)
    if (OVERWRITE_TESTDATA) {
      VfsTestUtil.overwriteTestData(expectedFileName, text)
      println(s"File $expectedFileName created.")
    }
    try {
      val expectedText = doLoadFile(fullPath, targetDataName)
      if (!Objects.equals(expectedText, text)) {
        throw new FileComparisonFailure(
          targetDataName, expectedText, text, expectedFileName
        )
      }
    } catch {
      case _: FileNotFoundException =>
        VfsTestUtil.overwriteTestData(expectedFileName, text)
        TestCase.fail(s"No output text found. File $expectedFileName created.")
    }
  }

  // TODO: @tailrec this instead of vars and while.
  private def checkSegments(text: CharSequence, lexer: Lexer): Unit = {
    lexer.start(text, 0, text.length())
    if (text.length == 0) {
      TestCase.assertNull("tokenType was not null", lexer.getTokenType)
      return
    }
    TestCase.assertNotNull("tokenType was null", lexer.getTokenType)
    var lastEnd = -1
    while (true) {
      if (lastEnd == -1) {
        TestCase.assertEquals(0, lexer.getTokenStart)
      } else {
        TestCase.assertEquals(lastEnd, lexer.getTokenStart)
      }
      lastEnd = lexer.getTokenEnd
      lexer.advance()
      if (lexer.getTokenType == null) return
    }
  }

  private def printTokensExtra(text: String, start: Int): String = {
    printTokensExtra(text, start, createLexer())
  }

  private def printTokensExtra(text: CharSequence, start: Int, lexer: Lexer): String = {
    lexer.start(text, start, text.length())
    var result = ""
    while (true) {
      val tokenType = lexer.getTokenType
      if (tokenType == null) return result
      val tokenText = getTokenText(lexer)
      val tokenTypeName = tokenType.toString
      val line = s"$tokenTypeName ('$tokenText') ${lexer.getTokenStart}-${lexer.getTokenEnd}\n"
      result += line
      lexer.advance()
    }
    throw new RuntimeException("Impossible")
  }

  /** Copy pasta from LexerTestCase.getTokenText. */
  private def getTokenText(lexer: Lexer): String = {
    lexer.getTokenType match {
      case tokenType: TokenWrapper => tokenType.getValue
      case _ =>
        StringUtil.replace(
          lexer.getBufferSequence.subSequence(
            lexer.getTokenStart, lexer.getTokenEnd
          ).toString,
          "\n", "\\n"
        )
    }
  }
}
