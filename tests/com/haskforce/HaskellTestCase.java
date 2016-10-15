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

package com.haskforce;

import com.haskforce.system.packages.GHCVersionTest;
import com.haskforce.system.packages.HPackageTest;
import com.haskforce.system.packages.ProjectSetupTest;
import com.haskforce.tools.cabal.AddCabalPackageTest;
import com.haskforce.tools.cabal.lang.parser.CabalParserTest;
import com.haskforce.tools.cabal.lang.lexer.CabalParsingLexerTest;
import com.haskforce.tools.cabal.resolve.CabalResolveTest;
import com.haskforce.tools.cabal.packages.CabalPackageTest;
import com.haskforce.haskell.codeInsight.HaskellCompletionTest;
import com.haskforce.haskell.codeInsight.HaskellFindUsagesTest;
import com.haskforce.haskell.codeInsight.HaskellGoToSymbolTest;
import com.haskforce.haskell.features.HaskellCommenterTest;
import com.haskforce.haskell.features.HaskellFoldingBuilderTest;
import com.haskforce.haskell.features.HaskellTypedHandlerTest;
import com.haskforce.haskell.highlighting.HaskellLexerTest;
import com.haskforce.tools.ghcmod.mod.GhcModTest;
import com.haskforce.tools.ghcmod.GhcModUtilTest;
import com.haskforce.tools.stack.importWizard.StackImportWizardTest;
import com.haskforce.tools.stack.importWizard.StackYamlTest;
import com.haskforce.haskell.parser.HaskellParserTest;
import com.haskforce.haskell.parser.HaskellParsingLexerTest;
import com.haskforce.haskell.projectWizard.NewProjectWizardTest;
import com.haskforce.haskell.refactoring.HaskellRenameTest;
import com.haskforce.haskell.resolve.HaskellResolveTest;
import com.haskforce.haskell.spellchecker.HaskellSpellcheckingTest;
import com.haskforce.system.utils.HtmlUtilsTest;
import junit.framework.TestCase;
import junit.framework.TestSuite;

/**
 * Main testsuite driver. Specifies which components that should be tested.
 * Test cases belong in the individual test suites.
 */
@SuppressWarnings("ALL")
public class HaskellTestCase extends TestCase {
    public static TestSuite suite() {
        TestSuite suite = new TestSuite();
        // TODO: The order of the tests seem to matter.  If the HaskellTypedHandlerTest does not come first it seems
        // to fail.  Once re-ordered, the HaskellParserTest.testInternalLexer seems to produce a different AST,
        // so there seems to be something strange going on.
        // See https://github.com/carymrobbins/intellij-haskforce/issues/63
        suite.addTestSuite(HaskellTypedHandlerTest.class);
        suite.addTestSuite(HaskellLexerTest.class);
        suite.addTestSuite(HaskellParsingLexerTest.class);
        suite.addTestSuite(HaskellParserTest.class);
        suite.addTestSuite(HaskellCommenterTest.class);
        suite.addTestSuite(HaskellFoldingBuilderTest.class);
        suite.addTestSuite(HaskellCompletionTest.class);
        suite.addTestSuite(HaskellFindUsagesTest.class);
        suite.addTestSuite(HaskellRenameTest.class);
        suite.addTestSuite(HaskellResolveTest.class);
        suite.addTestSuite(HaskellGoToSymbolTest.class);
        suite.addTestSuite(HaskellSpellcheckingTest.class);
        suite.addTestSuite(NewProjectWizardTest.class);


        //system
        suite.addTestSuite(GHCVersionTest.class);
        suite.addTestSuite(HPackageTest.class);
        suite.addTestSuite(HtmlUtilsTest.class);
        suite.addTestSuite(ProjectSetupTest.class);

        //tools

        // Cabal
        suite.addTestSuite(AddCabalPackageTest.class);
        suite.addTestSuite(CabalParsingLexerTest.class);
        suite.addTestSuite(CabalParserTest.class);
        suite.addTestSuite(CabalPackageTest.class);
        suite.addTestSuite(CabalResolveTest.class);

        //stack
        suite.addTestSuite(StackYamlTest.class);
        suite.addTestSuite(StackImportWizardTest.class);

        //ghcmod
        suite.addTestSuite(GhcModUtilTest.class);
        suite.addTestSuite(GhcModTest.class);
        return suite;
    }
}
