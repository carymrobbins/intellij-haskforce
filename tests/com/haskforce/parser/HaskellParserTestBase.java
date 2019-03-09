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

import com.intellij.lang.ParserDefinition;
import com.intellij.testFramework.ParsingTestCase;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.TestDataFile;
import junit.framework.AssertionFailedError;
import org.jetbrains.annotations.NonNls;

import java.io.File;
import java.io.IOException;

public abstract class HaskellParserTestBase extends ParsingTestCase {
    public HaskellParserTestBase(String dataPath, String fileExt, boolean lowercaseFirstLetter,
                                 ParserDefinition... definitions) {
        super(dataPath, fileExt, lowercaseFirstLetter, definitions);
    }

    @Override
    protected String getTestDataPath() {
        return "tests" + File.separator + "gold";
    }

    @Override
    protected boolean skipSpaces() {
        return true;
    }

    // NOTE: This is public to ensure our CabalParsingTestCases mixin works.
    public void doTest() {
        doTest(true, true);
    }

    /**
     * Perform a test. Add tests that should work but does not work yet with
     * doTest(false, false).
     */
    protected void doTest(boolean checkResult, boolean shouldPass) {
        // Do some gymnastics to see if we had to create the comparison file.
        // This is convenient so we can tell if the generated file has error elements or not.
        AssertionFailedError noComparisonTextFound = null;
        try {
            doTest(checkResult);
        } catch (AssertionFailedError e) {
            if (e.getMessage().startsWith("No output text found")) {
                boolean hasErrors = toParseTreeText(myFile, skipSpaces(), includeRanges()).contains("PsiErrorElement");
                if (hasErrors) fail(e.getMessage() + " (contains error elements)");
                fail(e.getMessage() + " (no error elements)");
            } else {
                throw e;
            }
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
     * outputs in tests/gold/parser/expected
     */
    @Override
    protected void checkResult(@NonNls @TestDataFile String targetDataName,
                               final PsiFile file) throws IOException {
        doCheckResult(myFullDataPath, file, checkAllPsiRoots(),
                "expected" + File.separator + targetDataName, skipSpaces(),
                includeRanges());
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
    }
}
