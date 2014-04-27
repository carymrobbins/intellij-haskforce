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

import com.intellij.core.CoreApplicationEnvironment;
import com.intellij.lang.LanguageExtensionPoint;
import com.intellij.lang.ParserDefinition;
import com.intellij.openapi.extensions.Extensions;
import com.intellij.testFramework.ParsingTestCase;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.TestDataFile;
import org.jetbrains.annotations.NonNls;

import java.io.File;
import java.io.IOException;

public abstract class HaskellParserTestBase extends ParsingTestCase {
    public HaskellParserTestBase(String dataPath, String fileExt, ParserDefinition... definitions) {
        super(dataPath, fileExt, definitions);
    }

    @Override
    protected String getTestDataPath() {
        return "tests" + File.separator + "gold";
    }

    @Override
    protected boolean skipSpaces() {
        return true;
    }

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
     * Expected outputs go <path>/<component>/expected/, putting the parser
     * outputs in test/gold/parser/expected.
     */
    @Override
    protected void checkResult(@NonNls @TestDataFile String targetDataName, final PsiFile file) throws IOException {
        doCheckResult(myFullDataPath, file, checkAllPsiRoots(), "expected" + File.separator + targetDataName, skipSpaces(), includeRanges());
    }

    @Override
    protected void setUp() throws Exception {
        super.setUp();
        CoreApplicationEnvironment.registerExtensionPoint(Extensions.getRootArea(), "com.intellij.lang.braceMatcher", LanguageExtensionPoint.class);
    }

}
