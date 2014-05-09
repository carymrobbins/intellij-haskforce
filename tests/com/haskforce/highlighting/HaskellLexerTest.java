package com.haskforce.highlighting;

/**
 * Lexer test driver. Add new lexer testcases here.
 */
public class HaskellLexerTest extends HaskellLexerTestBase {
    public HaskellLexerTest() {
        super();
    }

    /* Borrow the test inputs from ParserTest. */
    public void testFFI00001()          { doTest(true, true); }
    public void testHello00001()        { doTest(true, true); }
    public void testHello00002()        { doTest(true, true); }
    public void testHello00003()        { doTest(true, false); }
    public void testImport00001()       { doTest(true, true); }
    public void testImport00002()       { doTest(true, true); }
    public void testImport00003()       { doTest(true, true); }
    public void testImport00004()       { doTest(true, true); }
    public void testComment00001()      { doTest(true, true); }
    public void testComment00002()      { doTest(true, true); }
    public void testComment00003()      { doTest(true, true); }
    public void testLambda00001()       { doTest(true, true); }
    public void testPragma00001()       { doTest(true, true); }
    public void testString00001()       { doTest(true, true); }
    public void testString00002()       { doTest(true, true); }
    public void testString00003()       { doTest(true, true); }
    public void testString00004()       { doTest(true, false); }
    public void testString00005()       { doTest(true, true); }
    public void testTempHask00001()     { doTest(true, true); }
    public void testQuote00001()        { doTest(true, true); }
}