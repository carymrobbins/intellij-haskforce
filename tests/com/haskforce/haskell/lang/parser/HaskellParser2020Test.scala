package com.haskforce.haskell.lang.parser

import java.io.File

import com.haskforce.HaskellParserDefinition
import com.haskforce.parser.HaskellParserTestBase
import com.intellij.psi.PsiFile
import com.intellij.testFramework.{ParsingTestCase, TestDataFile}
import org.jetbrains.annotations.NonNls

class HaskellParser2020Test
  extends HaskellParserTestBase(
    "parser", "hs", false,
    new HaskellParserDefinition(
      HaskellParserDefinition.Mode.PARSER2020
    )
  ) {

  override protected def checkResult(
    @NonNls @TestDataFile targetDataName: String,
    file: PsiFile
  ): Unit = {
    ParsingTestCase.doCheckResult(
      myFullDataPath,
      file,
      checkAllPsiRoots(),
      "expected2020" + File.separator + targetDataName,
      skipSpaces(),
      includeRanges()
    )
  }

  def testImport00001(): Unit = doTest(true, true)
  def testImport00002(): Unit = doTest(true, true)
  def testModule00001(): Unit = doTest(true, true)
  def testModule00002(): Unit = doTest(true, true)

  // def testAStack00001(): Unit = doTest(true, true)
  // def testArrow00001(): Unit = doTest(true, true)
  // def testBlockArguments00001(): Unit = doTest(true, true)
  // def testCase00001(): Unit = doTest(true, true)
  // def testCPP00001(): Unit = doTest(true, true)
  // def testDefaultSignatures00001(): Unit = doTest(true, true)
  // def testDerivingStrategies00001(): Unit = doTest(true, true)
  // def testDerivingVia00001(): Unit = doTest(true, true)
  // def testExport00001(): Unit = doTest(true, true)
  // def testEta00001(): Unit = doTest(true, true)
  // def testFun00001(): Unit = doTest(true, true)
  // def testFun00002(): Unit = doTest(true, true)
  // def testFun00003(): Unit = doTest(true, true)
  // def testFun00004(): Unit = doTest(true, true)
  // def testFun00005(): Unit = doTest(true, true)
  // def testFun00006(): Unit = doTest(true, true)
  // def testFun00007(): Unit = doTest(true, true)
  // def testFun00008(): Unit = doTest(true, true)
  // def testFun00009(): Unit = doTest(true, true)
  // def testFun00010(): Unit = doTest(true, true)
  // def testFun00011(): Unit = doTest(true, true)
  // def testFun00012(): Unit = doTest(true, true)
  // def testFun00013(): Unit = doTest(true, true)
  // def testFFI00001(): Unit = doTest(true, true)
  // def testFFI00002(): Unit = doTest(true, true)
  // def testHello00001(): Unit = doTest(true, true)
  // def testHello00002(): Unit = doTest(true, true)
  // def testHello00003(): Unit = doTest(true, false)
  // def testImport00003(): Unit = doTest(true, true)
  // def testImport00004(): Unit = doTest(true, true)
  // def testImport00005(): Unit = doTest(true, true)
  // def testImport00006(): Unit = doTest(true, true)
  // def testImport00007(): Unit = doTest(true, true)
  // def testImport00008(): Unit = doTest(true, true)
  // def testInstance00001(): Unit = doTest(true, true)
  // def testInstance00002(): Unit = doTest(true, true)
  // def testInstanceSigs00001(): Unit = doTest(true, true)
  // def testInfix00001(): Unit = doTest(true, true)
  // def testKind00001(): Unit = doTest(true, true)
  // def testKind00003(): Unit = doTest(true, true)
  // def testKind00004(): Unit = doTest(true, true)
  // def testLayout00001(): Unit = doTest(true, true)
  // def testLayout00002(): Unit = doTest(true, true)
  // def testLayout00003(): Unit = doTest(true, true)
  // def testLayout00004(): Unit = doTest(true, true)
  // def testLayout00005(): Unit = doTest(true, true)
  // def testLayout00006(): Unit = doTest(true, true)
  // def testLayout00008(): Unit = doTest(true, true)
  // def testLayout00009(): Unit = doTest(true, false)
  // def testLayout00010(): Unit = doTest(true, true)
  // def testLayout00011(): Unit = doTest(true, true)
  // def testLayout00012(): Unit = doTest(true, true)
  // def testLayout00013(): Unit = doTest(true, true)
  // def testLayout00014(): Unit = doTest(true, true)
  // def testLayout00015(): Unit = doTest(true, true)
  // def testLayout00016(): Unit = doTest(true, true)
  // def testLayout00017(): Unit = doTest(true, true)
  // def testLayout00018(): Unit = doTest(true, true)
  // def testLayout00020(): Unit = doTest(true, true)
  // def testLayout00021(): Unit = doTest(true, true)
  // def testLayout00022(): Unit = doTest(true, true)
  // def testLayout00023(): Unit = doTest(true, true)
  // def testLayout00024(): Unit = doTest(true, true)
  // def testLayout00025(): Unit = doTest(true, true)
  // def testLayout00026(): Unit = doTest(true, true)
  // def testLet00001(): Unit = doTest(true, true)
  // def testList00001(): Unit = doTest(true, true)
  // def testList00002(): Unit = doTest(true, true)
  // def testComment00001(): Unit = doTest(true, true)
  // def testComment00002(): Unit = doTest(true, true)
  // def testComment00003(): Unit = doTest(true, true)
  // def testComment00006(): Unit = doTest(true, true)
  // def testComment00007(): Unit = doTest(true, true)
  // def testComment00008(): Unit = doTest(true, true)
  // def testLambda00001(): Unit = doTest(true, true)
  // def testLambdaCase00001(): Unit = doTest(true, true)
  // def testMagicHash00001(): Unit = doTest(true, true)
  // def testMinimal00001(): Unit = doTest(true, true)
  // def testOperator00001(): Unit = doTest(true, true)
  // def testOperator00002(): Unit = doTest(true, true)
  // def testRecord00001(): Unit = doTest(true, true)
  // def testRecord00002(): Unit = doTest(true, true)
  // def testRecord00003(): Unit = doTest(true, true)
  // def testPragma00001(): Unit = doTest(true, true)
  // def testPragma00002(): Unit = doTest(true, true)
  // def testPragma00003(): Unit = doTest(true, true)
  // def testPragma00004(): Unit = doTest(true, true)
  // def testPragma00005(): Unit = doTest(true, true)
  // def testProc00001(): Unit = doTest(true, true)
  // def testStrict00001(): Unit = doTest(true, true)
  // def testString00001(): Unit = doTest(true, true)
  // def testString00002(): Unit = doTest(true, true)
  // def testString00003(): Unit = doTest(true, true)
  // def testString00004(): Unit = doTest(true, false)
  // def testString00005(): Unit = doTest(true, true)
  // def testString00006(): Unit = doTest(true, true)
  // def testString00007(): Unit = doTest(true, true)
  // def testTempHask00001(): Unit = doTest(true, true)
  // def testTempHask00002(): Unit = doTest(true, true)
  // def testTempHask00003(): Unit = doTest(true, true)
  // def testTempHask00004(): Unit = doTest(true, true)
  // def testTempHask00005(): Unit = doTest(true, true)
  // def testUnicode00001(): Unit = doTest(true, true)
  // def testType00001(): Unit = doTest(true, true)
  // def testType00002(): Unit = doTest(true, true)
  // def testType00003(): Unit = doTest(true, true)
  // def testType00004(): Unit = doTest(true, true)
  // def testType00005(): Unit = doTest(true, true)
  // def testType00006(): Unit = doTest(true, true)
  // def testType00007(): Unit = doTest(true, true)
  // def testTypeApplications00001(): Unit = doTest(true, true)
  // def testTypeFamilies00001(): Unit = doTest(true, true)
  // def testVar00001(): Unit = doTest(true, true)
  // def testViewPatterns00001(): Unit = doTest(true, true)
  // def testQuote00001(): Unit = doTest(true, true)
  // def testForAll00001(): Unit = doTest(true, true)

  // TODO: These don't work in the original parser
  // def testLayout00019() = doTest(true, true)
  // def testKind00002() = doTest(true, true)
}
