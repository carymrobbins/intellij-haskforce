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

    public void testFFI00001()          { doTest(true, true); }
    public void testHello00001()        { doTest(true, true); }
    public void testHello00002()        { doTest(true, true); }
    public void testHello00003()        { doTest(true, false); }
    public void testImport00001()       { doTest(true, true); }
    public void testImport00002()       { doTest(true, true); }
    public void testImport00003()       { doTest(true, true); }
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