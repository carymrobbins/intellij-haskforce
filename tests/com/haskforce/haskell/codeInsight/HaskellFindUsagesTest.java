/*
 * Copyright 2012-2014 Sergey Ignatov
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

package com.haskforce.haskell.codeInsight;

import com.haskforce.haskell.HaskellLightPlatformCodeInsightFixtureTestCase;

/**
 * Typed handler test driver. Add new typed handler testcases here.
 */
public class HaskellFindUsagesTest extends HaskellLightPlatformCodeInsightFixtureTestCase {
    public HaskellFindUsagesTest() {
        super("codeInsight", "codeInsight");
    }

    public void testFunctionUsagesInSingleFile00001()    { doTest(3); }
    public void testFunctionUsagesInMultipleFiles00001() { doTest(3, "FunctionUsagesInSingleFile00001.hs");}
    public void testFunctionUsagesInSingleFile00002()    { doTest(2); }

    private void doTest(int expectedResult, String ... extraFiles) {
        String[] files = new String[1 + extraFiles.length];
        files[0] = getTestName(false) + ".hs";
        System.arraycopy(extraFiles, 0, files, 1, extraFiles.length);
        assertEquals(expectedResult, myFixture.testFindUsages(files).size());
    }
}
