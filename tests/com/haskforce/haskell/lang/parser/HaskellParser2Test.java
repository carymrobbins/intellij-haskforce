package com.haskforce.haskell.lang.parser;

import com.haskforce.parser.HaskellParserTestBase;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.TestDataFile;
import com.intellij.testFramework.UsefulTestCase;
import org.jetbrains.annotations.NonNls;

import java.io.File;
import java.io.IOException;

public class HaskellParser2Test extends HaskellParserTestBase {
  public HaskellParser2Test() {
    super("parser", "hs", false, new HaskellParserDefinition2());
  }

  // Mostly a hack to get the parser2 trees to go to their own directory.
  // This stuff is way too coupled.
  @Override
  protected void checkResult(@NonNls @TestDataFile String targetDataName,
                             final PsiFile file) throws IOException {
    doCheckResult(myFullDataPath, file, checkAllPsiRoots(),
      "../parser2/expected" + File.separator + targetDataName, skipSpaces(),
      includeRanges());
  }

  // See comment in HaskellLexerTest.java.
  public void testAStack00001()       { doTest(true, true); }

  public void testArrow00001()        { doTest(true, true); }
  public void testCase00001()         { doTest(true, true); }
  public void testCPP00001()          { doTest(true, true); }
  public void testExport00001()       { doTest(true, true); }
  public void testEta00001()          { doTest(true, true); }
  public void testFun00001()          { doTest(true, true); }
  public void testFun00002()          { doTest(true, true); }
  public void testFun00003()          { doTest(true, true); }
  public void testFun00004()          { doTest(true, true); }
  public void testFun00005()          { doTest(true, true); }
  public void testFun00006()          { doTest(true, true); }
  public void testFun00007()          { doTest(true, true); }
  public void testFun00008()          { doTest(true, true); }
  public void testFun00009()          { doTest(true, true); }
  public void testFun00010()          { doTest(true, true); }
  public void testFun00011()          { doTest(true, true); }
  public void testFun00012()          { doTest(true, true); }
  public void testFun00013()          { doTest(true, true); }
  public void testFFI00001()          { doTest(true, true); }
  public void testFFI00002()          { doTest(true, true); }
  public void testHello00001()        { doTest(true, true); }
  public void testHello00002()        { doTest(true, true); }
  public void testHello00003()        { doTest(true, false); }
  public void testImport00001()       { doTest(true, true); }
  public void testImport00002()       { doTest(true, true); }
  public void testImport00003()       { doTest(true, true); }
  public void testImport00004()       { doTest(true, true); }
  public void testImport00005()       { doTest(true, true); }
  public void testImport00006()       { doTest(true, true); }
  public void testInstanceSigs00001() { doTest(true, true); }
  public void testInfix00001()        { doTest(true, true); }
  public void testKind00001()         { doTest(true, true); }
  // FIXME: Disabled for now. Fails JSON comparison. Line looks truncated.
  // public void testKind00002()         { doTest(true, true); }
  public void testKind00003()         { doTest(true, true); }
  public void testKind00004()         { doTest(true, true); }
  public void testLayout00001()       { doTest(true, true); }
  public void testLayout00002()       { doTest(true, true); }
  public void testLayout00003()       { doTest(true, true); }
  public void testLayout00004()       { doTest(true, true); }
  public void testLayout00005()       { doTest(true, true); }
  public void testLayout00006()       { doTest(true, true); }
  public void testLayout00008()       { doTest(true, true); }
  public void testLayout00009()       { doTest(true, false); }
  public void testLayout00010()       { doTest(true, true); }
  public void testLayout00011()       { doTest(true, true); }
  public void testLayout00012()       { doTest(true, true); }
  public void testLayout00013()       { doTest(true, true); }
  public void testLayout00014()       { doTest(true, true); }
  public void testLayout00015()       { doTest(true, true); }
  public void testLayout00016()       { doTest(true, true); }
  public void testLayout00017()       { doTest(true, true); }
  public void testLayout00018()       { doTest(true, true); }
  // FIXME: Layout00019.hs fails to parse.
  // public void testLayout00019()       { doTest(true, true); }
  public void testLayout00020()       { doTest(true, true); }
  public void testLayout00021()       { doTest(true, true); }
  public void testLayout00022()       { doTest(true, true); }
  public void testLayout00023()       { doTest(true, true); }
  public void testLayout00024()       { doTest(true, true); }
  public void testLayout00025()       { doTest(true, true); }
  public void testLet00001()          { doTest(true, true); }
  public void testList00001()         { doTest(true, true); }
  public void testList00002()         { doTest(true, true); }
  public void testComment00001()      { doTest(true, true); }
  public void testComment00002()      { doTest(true, true); }
  public void testComment00003()      { doTest(true, true); }
  public void testComment00006()      { doTest(true, true); }
  public void testComment00007()      { doTest(true, true); }
  public void testComment00008()      { doTest(true, true); }
  public void testLambda00001()       { doTest(true, true); }
  public void testMagicHash00001()    { doTest(true, true); }
  public void testMinimal00001()      { doTest(true, true); }
  public void testModule00001()       { doTest(true, true); }
  public void testOperator00001()     { doTest(true, true); }
  public void testOperator00002()     { doTest(true, true); }
  public void testRecord00001()       { doTest(true, true); }
  public void testRecord00002()       { doTest(true, true); }
  public void testPragma00001()       { doTest(true, true); }
  public void testPragma00002()       { doTest(true, true); }
  public void testPragma00003()       { doTest(true, true); }
  public void testPragma00004()       { doTest(true, true); }
  public void testPragma00005()       { doTest(true, true); }
  public void testProc00001()         { doTest(true, true); }
  public void testStrict00001()       { doTest(true, true); }
  public void testString00001()       { doTest(true, true); }
  public void testString00002()       { doTest(true, true); }
  public void testString00003()       { doTest(true, true); }
  public void testString00004()       { doTest(true, false); }
  public void testString00005()       { doTest(true, true); }
  public void testString00006()       { doTest(true, true); }
  public void testString00007()       { doTest(true, true); }
  public void testTempHask00001()     { doTest(true, true); }
  public void testTempHask00002()     { doTest(true, true); }
  public void testTempHask00003()     { doTest(true, true); }
  public void testTempHask00004()     { doTest(true, true); }
  public void testTempHask00005()     { doTest(true, true); }
  public void testUnicode00001()      { doTest(true, true); }
  public void testType00001()         { doTest(true, true); }
  public void testType00002()         { doTest(true, true); }
  public void testType00003()         { doTest(true, true); }
  public void testType00004()         { doTest(true, true); }
  public void testType00005()         { doTest(true, true); }
  public void testType00006()         { doTest(true, true); }
  public void testVar00001()          { doTest(true, true); }
  public void testViewPatterns00001() { doTest(true, true); }
  public void testQuote00001()        { doTest(true, true); }
  public void testForAll00001()        { doTest(true, true); }
  // public void testParseMonad()        { doTest(true, true); }

  // TODO: Change to doTest(true, true) after bug fixes.
  // Note that testInternalLexer seems to output different parse trees between IntelliJ 13 and 14.
  // This is probably due to the excessive DUMMY_BLOCKs throughout the parse result.
  // public void testInternalLexer()     { doTest(true, false); }
}
