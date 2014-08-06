package com.haskforce.parser;

import com.haskforce.highlighting.HaskellLexerTestBase;
import com.haskforce.parsing.HaskellParsingLexer;
import com.intellij.lexer.Lexer;

/**
 * Parsing lexer test driver. Add new parsing lexer testcases here.
 */
public class HaskellParsingLexerTest extends HaskellLexerTestBase {
    public HaskellParsingLexerTest() {
        super("parsingLexer");
    }

    @Override
    protected Lexer createLexer() {
        return new HaskellParsingLexer();
    }

    /* Layout test from Chapter 2 in Haskell 2010 report.
     * We get the braces/parentheses at the end of "pop" wrong.
     * http://ogi.altocumulus.org/~hallgren/hsutils/ explains
     * why we can not lex the same way as the standard mandates. Section 10.3,
     * note 6 in Haskell 2010 give more details about parser-lexer interaction.
     */
    public void testAStack00001()      { doTest(true, true); }

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
    public void testImport00005()       { doTest(true, true); }
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
    public void testLayout00009()       { doTest(true, true); }
    public void testLayout00010()       { doTest(true, true); }
    public void testLayout00011()       { doTest(true, true); }
    public void testLayout00012()       { doTest(true, true); }
    public void testComment00001()      { doTest(true, true); }
    public void testComment00002()      { doTest(true, true); }
    public void testComment00003()      { doTest(true, true); }
    public void testComment00004()      { doTest(true, true); }
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
}
