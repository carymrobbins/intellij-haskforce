package com.haskforce.haskell.resolve;

/**
 * Test class for resolving references.  To add a new test, add a method to this class following the conventions below.
 *  - If your test is named `testThing` then you should create a `Thing` directory under `tests/gold/resolve`.
 *  - Add Haskell files to the new directory.
 *  - In your files, insert "<ref>" right before the element you want to resolve.
 *  - Insert "<resolved>" right before the element you wish for the reference to resolve to.
 */
public class HaskellResolveTest extends HaskellResolveTestCase {
    public void testClass00001() { doTest(); }
    public void testClass00002() { doTest(); }
    public void testData00001() { doTest(); }
    public void testData00002() { doTest(false); }
    public void testData00003() { doTest(false); }
    public void testData00004() { doTest(false); }
    public void testData00005() { doTest(false); }
    public void testData00006() { doTest(false); }
    public void testData00007() { doTest(); }
    public void testExportFunction00001() { doTest(); }
    public void testExportFunction00002() { doTest(); }
    public void testFunctionWithoutSignature00001() { doTest(); }
    public void testFunctionWithoutSignature00002() { doTest(); }
    public void testFunctionWithSignature00001() { doTest(); }
    public void testNewtype00001() { doTest(); }
    public void testModule00001() { doTest(); }
    public void testModule00002() { doTest(); }
    public void testModule00003() { doTest(); }
    public void testModule00004() { doTest(); }
    public void testType00001() { doTest(); }
}
