package com.haskforce.cabal.lang.parser

/** Provides test suites to both Lexers and Parsers for Cabal. */
trait CabalParsingTestCases {

  def testBlank00001() = doTest()
  def testBraces00001() = doTest()
  def testExample00001() = doTest()
  def testExample00002() = doTest()
  def testExample00003() = doTest()
  def testExample00004() = doTest()
  def testExample00005() = doTest()
  def testExample00006() = doTest()
  def testExample00007() = doTest()
  def testExample00008() = doTest()
  def testExample00009() = doTest()
  def testExample00010() = doTest()
  def testExample00011() = doTest()
  def testExample00012() = doTest()
  def testIf00001() = doTest()
  def testIf00002() = doTest()

  def doTest(): Unit
}
