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

package com.haskforce.haskell.features;

import com.haskforce.haskell.HaskellLightPlatformCodeInsightFixtureTestCase;

/**
 * Typed handler test driver. Add new typed handler testcases here.
 */
public class HaskellTypedHandlerTest extends HaskellLightPlatformCodeInsightFixtureTestCase {
    public HaskellTypedHandlerTest() {
        super("features", "features");
    }

    public void testNotPaired() throws Throwable {
        doTest('(', "foo = <caret>a", "foo = (<caret>a");
    }

    public void testPaired() throws Throwable {
        doTest('(', "foo <caret>", "foo (<caret>)");
    }

    public void testTypeOverCloseBracket() throws Throwable {
        doTest(')', "foo (<caret>)", "foo ()<caret>");
    }

    private void doTest(char c, String before, String after) {
        myFixture.configureByText("A.hs", before);
        myFixture.type(c);
        myFixture.checkResult(after);
    }
}
