/*
 * Copyright 2012-2013 Sergey Ignatov
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/*
 * Adapted from ErlangParserTest.java. Downloaded 21 Apr 2014 from:
 *
 * https://github.com/ignatov/intellij-erlang.
 */

package com.haskforce.parser;

import com.haskforce.HaskellParserDefinition;

public class HaskellParserTest extends HaskellParserTestBase {
    public HaskellParserTest() {
        super("parser", "hs", new HaskellParserDefinition());
    }

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
    // TODO: Change to doTest(true, true) when we have the new parser.
    public void testFun00011()          { doTest(true, false); }
    public void testFun00012()          { doTest(true, true); }
    public void testFFI00001()          { doTest(true, true); }
    public void testFFI00002()          { doTest(true, true); }
    public void testHello00001()        { doTest(true, true); }
    public void testHello00002()        { doTest(true, true); }
    public void testHello00003()        { doTest(true, false); }
    public void testImport00001()       { doTest(true, true); }
    public void testImport00002()       { doTest(true, true); }
    public void testImport00003()       { doTest(true, true); }
    // TODO: Enable when we have a GHC-API parser.
    // public void testImport00004()       { doTest(true, true); }
    public void testKind00001()         { doTest(true, true); }
    // FIXME: Disabled for now. Fails JSON comparison. Line looks truncated.
    // public void testKind00002()         { doTest(true, true); }
    // TODO: Change to doTest(true, true) when we have the new parser.
    public void testKind00003()         { doTest(true, false); }
    public void testKind00004()         { doTest(true, true); }
    public void testComment00001()      { doTest(true, true); }
    public void testComment00002()      { doTest(true, true); }
    public void testComment00003()      { doTest(true, true); }
    public void testLambda00001()       { doTest(true, true); }
    public void testPragma00001()       { doTest(true, true); }
    public void testPragma00002()       { doTest(true, true); }
    // TODO: Change to doTest(true, true) when we have the new parser.
    public void testPragma00003()       { doTest(true, false); }
    // TODO: Change to doTest(true, true) when we have the new parser.
    public void testPragma00004()       { doTest(true, false); }
    public void testString00001()       { doTest(true, true); }
    public void testString00002()       { doTest(true, true); }
    public void testString00003()       { doTest(true, true); }
    public void testString00004()       { doTest(true, false); }
    public void testString00005()       { doTest(true, true); }
    public void testTempHask00001()     { doTest(true, true); }
    public void testTempHask00002()     { doTest(true, false); }
    public void testQuote00001()        { doTest(true, true); }
}