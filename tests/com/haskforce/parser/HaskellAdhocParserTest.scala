package com.haskforce.parser

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths}

import com.haskforce.HaskellLanguage
import com.haskforce.parsing.HaskellParsingLexer
import com.intellij.openapi.project.ProjectManager
import com.intellij.psi.PsiFileFactory
import com.intellij.psi.impl.DebugUtil
import com.intellij.testFramework.LexerTestCase
import com.intellij.util.text.ByteArrayCharSequence

object HaskellAdhocParserTest {

  private val ENV_IN_FILE = "HASKFORCE_ADHOC_PARSER_IN_FILE"
  private val ENV_OUT_PREFIX = "HASKFORCE_ADHOC_PARSER_OUT_PREFIX"
  private val ENV_SKIP_WHITESPACES = "HASKFORCE_ADHOC_PARSER_SKIP_WHITESPACES"

  def test(): Unit = {
    val inFile = sys.props.getOrElse(ENV_IN_FILE, {
      System.err.println(s"Missing -D$ENV_IN_FILE=... property")
      return
    })
    val outPrefix = sys.props.getOrElse(ENV_OUT_PREFIX, {
      System.err.println(s"Missing -D$ENV_OUT_PREFIX=... property")
      return
    })
    val skipWhiteSpaces = sys.props.get(ENV_SKIP_WHITESPACES) match {
      case None => false
      case Some("0") => false
      case Some("1") => false
      case Some(other) =>
        System.err.println(s"Invalid value for $ENV_SKIP_WHITESPACES; expected 0 or 1")
        System.exit(1)
        return
    }

    val bytes = Files.readAllBytes(Paths.get(inFile))
    val contents = new ByteArrayCharSequence(bytes, 0, bytes.length)

    val lexer = new HaskellParsingLexer
    val lexerOutFile = outPrefix + "-lexer.txt"
    val lexerOutput =
      LexerTestCase.printTokens(contents, 0, lexer)
          .getBytes(StandardCharsets.UTF_8)

    Files.write(Paths.get(lexerOutFile), lexerOutput)
    println("Lexer output written to " + lexerOutFile)

    val parser = new HaskellParser
    val parserOutFile = outPrefix + "-parser.txt"
    val psiOutput =
      PsiFileFactory
        .getInstance(ProjectManager.getInstance().getDefaultProject)
        .createFileFromText(HaskellLanguage.INSTANCE, contents)
    val parserOutput =
      DebugUtil.psiToString(psiOutput, skipWhiteSpaces)
        .getBytes(StandardCharsets.UTF_8)

    Files.write(Paths.get(parserOutFile), parserOutput)
    println("Parser output written to" + parserOutFile)
  }
}
