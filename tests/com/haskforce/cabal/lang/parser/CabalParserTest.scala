package com.haskforce.cabal.lang.parser

class CabalParserTest extends CabalParserTestBase with CabalParsingTestCases {
  // Allow error elements in this test
  override def testError00001() = doTestPermitErrors()
}
