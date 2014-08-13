package com.haskforce.highlighting;

/**
 * Lexer test driver. Add new lexer testcases here.
 */
public class HaskellLexerTest extends HaskellLexerTestBase {
    public HaskellLexerTest() {
        super("lexer");
    }

    /* Borrow the test inputs from ParserTest. */
    public void testArrow00001()        { doTest(true, true); }
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
    public void testFFI00001()          { doTest(true, true); }
    public void testFFI00002()          { doTest(true, true); }
    public void testHello00001()        { doTest(true, true); }
    public void testHello00002()        { doTest(true, true); }
    public void testHello00003()        { doTest(true, false); }
    public void testImport00001()       { doTest(true, true); }
    public void testImport00002()       { doTest(true, true); }
    public void testImport00003()       { doTest(true, true); }
    public void testImport00004()       { doTest(true, true); }
    public void testKind00001()         { doTest(true, true); }
    public void testKind00002()         { doTest(true, true); }
    public void testKind00003()         { doTest(true, true); }
    public void testKind00004()         { doTest(true, true); }
    public void testLayout00001()       { doTest(true, true); }
    public void testLayout00002()       { doTest(true, true); }
    public void testLayout00003()       { doTest(true, true); }
    public void testLayout00004()       { doTest(true, true); }
    public void testLayout00005()       { doTest(true, true); }
    public void testLayout00006()       { doTest(true, true); }
    public void testLayout00007()       { doTest(true, true); }
    public void testLayout00008()       { doTest(true, true); }
    public void testLayout00011()       { doTest(true, true); }
    public void testLayout00012()       { doTest(true, true); }
    public void testLayout00013()       { doTest(true, true); }
    public void testLayout00014()       { doTest(true, true); }
    public void testLayout00015()       { doTest(true, true); }
    public void testLayout00016()       { doTest(true, true); }
    public void testLayout00017()       { doTest(true, true); }
    public void testComment00001()      { doTest(true, true); }
    public void testComment00002()      { doTest(true, true); }
    public void testComment00003()      { doTest(true, true); }
    public void testLambda00001()       { doTest(true, true); }
    public void testPragma00001()       { doTest(true, true); }
    public void testPragma00002()       { doTest(true, true); }
    public void testPragma00003()       { doTest(true, true); }
    public void testPragma00004()       { doTest(true, true); }
    public void testString00001()       { doTest(true, true); }
    public void testString00002()       { doTest(true, true); }
    public void testString00003()       { doTest(true, true); }
    public void testString00004()       { doTest(true, false); }
    public void testString00005()       { doTest(true, true); }
    public void testTempHask00001()     { doTest(true, true); }
    public void testTempHask00002()     { doTest(true, true); }
    public void testQuote00001()        { doTest(true, true); }
    public void testInternalLexer()     { doTest(true, true); }
}