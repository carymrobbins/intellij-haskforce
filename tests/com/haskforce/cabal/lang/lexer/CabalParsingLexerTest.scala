package com.haskforce.cabal.lang.lexer

import com.intellij.lexer.Lexer

import com.haskforce.cabal.lang.parser.CabalParsingTestCases

class CabalParsingLexerTest
  extends CabalLexerTestBase
  with CabalParsingTestCases {

  override def createLexer(): Lexer = new CabalParsingLexer
  override protected val getExpectedPath: String = "lexer"
}
