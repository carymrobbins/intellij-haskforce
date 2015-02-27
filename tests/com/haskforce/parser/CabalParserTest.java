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

import com.haskforce.cabal.CabalParserDefinition;

public class CabalParserTest extends CabalParserTestBase {
    public CabalParserTest() {
        super("cabal/parser", "cabal", new CabalParserDefinition());
    }

    // See comment in HaskellLexerTest.java.
    public void testSimpleKey()       { doTest(true, true); }
    public void testComplexKey()       { doTest(true, true); }
    public void testTwoSimpleKeys()       { doTest(true, true); }
    public void testSimpleLibrary()       { doTest(true, true); }
    public void testSimpleKeyAfterSimpleLibrary()       { doTest(true, true); }
    public void testLittleBiggerLibrary()       { doTest(true, true); }
    public void testVersionKey()       { doTest(true, true); }
    public void testBuildDepends()       { doTest(true, true); }
    public void testSimpleExecutable()       { doTest(true, true); }
    public void testDataDir()       { doTest(true, true); }
    public void testHomepage()       { doTest(true, true); }
    public void testComments()       { doTest(true, true); }
    public void testFreeFormWithIndent()       { doTest(true, true); }
    public void testOtherModules()       { doTest(true, true); }
    public void testFeldsparLanguage()       { doTest(true, true); }
    public void testHaxl()       { doTest(true, true); }
    public void testFlags()       { doTest(true, true); }
    public void testIfStatement()       { doTest(true, true); }
    public void testIfElseStatement()       { doTest(true, true); }
}
