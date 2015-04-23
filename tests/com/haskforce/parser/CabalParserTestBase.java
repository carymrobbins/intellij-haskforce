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
 * Adapted from ErlangParserTestBase.java. Downloaded 21 Apr 2014 from:
 *
 * https://github.com/ignatov/intellij-erlang.
 */

package com.haskforce.parser;

import com.haskforce.cabal.CabalLanguage;
import com.haskforce.parsing.jsonParser.JsonParser;
import com.intellij.lang.ParserDefinition;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.ParsingTestCase;
import com.intellij.testFramework.TestDataFile;
import org.jetbrains.annotations.NonNls;

import java.io.File;
import java.io.IOException;

public abstract class CabalParserTestBase extends ParsingTestCase {
    private JsonParser jsonParser;

    public CabalParserTestBase(String dataPath, String fileExt, ParserDefinition... definitions) {
        super(dataPath, fileExt, definitions);
        /**
         * Refer to cabal instance to make sure that the language is known
         * to the environment before the test is initialized. It seems to be
         * a bit dodgy that this is necessary, but if I do no do this
         * the test will always fail because it doesn't find the cabal language, unless I
         * start 'breaking' through the code, then it works. Meh..
         */
        CabalLanguage instance = CabalLanguage.INSTANCE;
    }

    @Override
    protected String getTestDataPath() {
        return "tests" + File.separator + "gold";
    }

    @Override
    protected boolean skipSpaces() {
        return true;
    }

    /**
     * Perform a test. Add tests that should work but does not work yet with
     * doTest(false, false).
     */
    protected void doTest(boolean checkResult, boolean shouldPass) {
        doTest(true);
        if (shouldPass) {
            assertFalse(
                    "PsiFile contains error elements",
                    toParseTreeText(myFile, skipSpaces(), includeRanges()).contains("PsiErrorElement")
            );
        }
    }

    /*
     * Ensure that expected outputs live in some other directory than the test
     * inputs.
     *
     * Additionally this function performs the JSON parser test because this
     * is a convenient place to hook into the testing.
     *
     * Expected outputs go <path>/<component>/expected/, putting the parser
     * outputs in tests/gold/parser/expected and jsonParser outputs in
     * tests/gold/parser/expectedJson.
     */
    @Override
    protected void checkResult(@NonNls @TestDataFile String targetDataName,
                               final PsiFile file) throws IOException {
        doCheckResult(myFullDataPath, file, checkAllPsiRoots(),
                "expected" + File.separator + targetDataName, skipSpaces(),
                includeRanges());
/* TODO: Re-enable if we return to parser-helper.
        String phPath = ExecUtil.locateExecutableByGuessing("parser-helper");
        if (phPath != null && !phPath.isEmpty()) {
            String expectedFile = myFullDataPath + File.separator +
                    "expectedJson" + File.separator + targetDataName + ".txt";
            assertSameLinesWithFile(expectedFile,
                    jsonParser.getJson(file.getText(), phPath));
        } else {
            assertEquals(true, false); // Always false.
        }
*/
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        jsonParser = new JsonParser(myProject);
    }
}
