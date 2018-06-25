package com.haskforce.highlighting.annotation.external

import com.haskforce.test.AssertMixin
import com.intellij.testFramework.UsefulTestCase

class HLintParserTest extends UsefulTestCase with AssertMixin {

  // HLint broke backwards compatibility as of this commit -
  // https://github.com/ndmitchell/hlint/commit/7bb10df6871759704a287fedea623f5142ad2154#diff-c1ba1f9735f9cb9bfe8be220568d211b
  def testParseBeforeAndAfter_2_1_5(): Unit = {
    import fixture_2_1_5._

    HLint.parseProblemsJson(before_2_1_5) match {
      case Left(e) => throw new AssertionError(e)
      case Right(p) =>
        assertEquals(1, p.size())
        assertSameElements(p, expected)
    }

    HLint.parseProblemsJson(at_2_1_5) match {
      case Left(e) => throw new AssertionError(e)
      case Right(p) =>
        assertEquals(1, p.size())
        assertSameElements(p, expected)
    }
  }

  private object fixture_2_1_5 {
    val before_2_1_5 = """[{"module":"Main","decl":"main","severity":"Warning","hint":"Redundant do","file":"Main.hs","startLine":4,"startColumn":8,"endLine":5,"endColumn":25,"from":"do putStrLn \"hello world\"","to":"putStrLn \"hello world\"","note":[],"refactorings":"[Replace {rtype = Expr, pos = SrcSpan {startLine = 4, startCol = 8, endLine = 5, endCol = 25}, subts = [(\"y\",SrcSpan {startLine = 5, startCol = 3, endLine = 5, endCol = 25})], orig = \"y\"}]"}]"""
    val at_2_1_5 = """[{"module":["Main"],"decl":["main"],"severity":"Warning","hint":"Redundant do","file":"Main.hs","startLine":4,"startColumn":8,"endLine":5,"endColumn":25,"from":"do putStrLn \"hello world\"","to":"putStrLn \"hello world\"","note":[],"refactorings":"[Replace {rtype = Expr, pos = SrcSpan {startLine = 4, startCol = 8, endLine = 5, endCol = 25}, subts = [(\"y\",SrcSpan {startLine = 5, startCol = 3, endLine = 5, endCol = 25})], orig = \"y\"}]"}]"""
    val expected = new HLint.Problem(
      "main",
      "Main.hs",
      "Redundant do",
      "do putStrLn \"hello world\"",
      "putStrLn \"hello world\"",
      "Main",
      Array.empty,
      "Warning",
      4,
      8,
      5,
      25,
      true
    )
  }
}
