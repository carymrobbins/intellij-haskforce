package com.haskforce.codeInsight;

import com.haskforce.psi.HaskellPsiUtil;
import com.haskforce.psi.impl.HaskellElementFactory;

import static com.haskforce.codeInsight.HaskellCompletionContributor.*;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.psi.PsiFile;
import com.intellij.util.containers.ContainerUtil;
import org.junit.Assert;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;

/**
 * Completion test driver. Add new completion test cases here.
 */
public class HaskellCompletionTest extends HaskellCompletionTestBase {
    public void testKeywordImport() throws Throwable {
        // Top level should work fine.
        doTestInclude("<caret>", "import ");
        // As well as after a module declaration.
        doTestInclude(
                "module Main where\n" +
                "<caret>",
                    "import ");
        // After multiple imports.
        doTestInclude(
                "import Foo\n" +
                "import qualified Bar\n" +
                "<caret>",
                    "import ");
        // After an appropriate pragma.
        doTestInclude(
                "import Foo\n" +
                "{-# ANN module \"foo\" #-}\n" +
                "<caret>",
                    "import ");
        // After a CPP.
        doTestInclude(
                "import Foo\n" +
                "#if foo\n" +
                "<caret>",
                    "import ");
        // But not after we've started definitions.
        doTestExclude(
                "import Foo\n" +
                "foo = 1\n" +
                "<caret>",
                    "import ");
        // Not even after a pragma.
        doTestExclude(
                "import Foo\n" +
                "foo = 1\n" +
                "{-# ANN module \"foo\" #-}\n" +
                "<caret>",
                    "import ");
        // Not in an expression.
        doTestExclude(
                "import Foo\n" +
                "foo = <caret>",
                    "import ");
    }

    public void testKeywordQualified() throws Throwable {
        doTestInclude("import <caret>", "qualified ");
    }

    public void testKeywordDo() throws Throwable {
        doTestInclude(
                "dayFractionToTimeOfDay = undefined\n" +
                "bar = do<caret>",
                    "do", "dayFractionToTimeOfDay");
    }

    public void testPragmaTypes() throws Throwable {
        doTestInclude("{-# <caret> #-}",  getPragmaTypes());
    }

    public void testLanguages() throws Throwable {
        final String[] fakeLangs = {"OverloadedStrings", "TypeFamilies", "OverloadedRecordFields"};
        loadLanguageExtensions(fakeLangs);
        doTestInclude("{-# LANGUAGE <caret> #-}", fakeLangs);
    }

    public void testGhcFlags() throws Throwable {
        final String[] fakeFlags = {"-ferror-spans", "-fno-error-spans", "-fprint-explicit-foralls"};
        loadGhcFlags(fakeFlags);
        doTestInclude("{-# OPTIONS_GHC <caret> #-}", fakeFlags);
        clearCache();
        // Strip the first "-" from each flag.
        String[] expected = new String[fakeFlags.length];
        for (int i = 0; i < fakeFlags.length; ++i) {
            expected[i] = fakeFlags[i].substring(1);
        }
        doTestInclude("{-# OPTIONS_GHC -<caret> #-}", expected);
    }

    public void testModuleImports() throws Throwable {
        final String[] fakeModules = {"Control.Monad", "Data.ByteString", "Data.Byteable"};
        loadVisibleModules(fakeModules);
        doTestInclude("import <caret>", "Control", "Data");
        doTestInclude("import Data.<caret>", "ByteString", "Byteable");
        doTestExclude("import Data.<caret>", "import ");
    }

    public void testNameImports() throws Throwable {
        FakeBrowseCache fakeBrowseCache = new FakeBrowseCacheBuilder()
          .put("Data.Ord", "Down", "EQ", "GT", "LT", "Ord", "Ordering", "compare", "comparing")
          .build();
        loadModuleSymbols(fakeBrowseCache);
        doTestInclude("import Data.Ord (<caret>)", fakeBrowseCache.get("Data.Ord"));
        doTestInclude("import Data.Ord (c<caret>)", "compare", "comparing");
        doTestInclude("import Data.Ord (Or<caret>)", "Ord", "Ordering");
        doTestInclude("import Data.Ord as Foo (Or<caret>)", "Ord", "Ordering");
    }

    public void testImportParsing() throws Throwable {
        PsiFile psiFile = HaskellElementFactory.createFileFromText(myFixture.getProject(),
                "import qualified Data.ByteString.Char8 as C\n" +
                "import qualified Control.Monad\n" +
                "import Data.Maybe\n" +
                "import Control.Applicative (liftA, liftA2, pure)\n" +
                "import Prelude hiding (Maybe(Just, Nothing))\n" +
                "import Control.Arrow ()\n" +
                "import Data.Either as E");
        List<HaskellPsiUtil.Import> actuals = HaskellPsiUtil.parseImports(psiFile);
        List<HaskellPsiUtil.Import> expecteds = Arrays.asList(
                HaskellPsiUtil.Import.qualifiedAs("Data.ByteString.Char8", "C", false, null),
                HaskellPsiUtil.Import.qualified("Control.Monad", false, null),
                HaskellPsiUtil.Import.global("Data.Maybe", false, null),
                HaskellPsiUtil.Import.global("Control.Applicative", false, new String[]{"liftA", "liftA2", "pure"}),
                HaskellPsiUtil.Import.global("Prelude", true, new String[]{"Maybe", "Just", "Nothing"}),
                HaskellPsiUtil.Import.global("Control.Arrow", false, new String[]{}),
                HaskellPsiUtil.Import.globalAs("Data.Either", "E", false, null));
        Assert.assertArrayEquals(expecteds.toArray(), actuals.toArray());
    }

    public void testQualifiedNames() throws Throwable {
        FakeBrowseCache fakeBrowseCache = new FakeBrowseCacheBuilder()
          .put("Data.ByteString.Char8", "ByteString", "all", "any", "append", "appendFile", "break")
          .put("C", "ByteString", "all", "any", "append", "appendFile", "break")
          .build();
        loadModuleSymbols(fakeBrowseCache);
        doTestInclude(
                "import qualified Data.ByteString.Char8 as C\n" +
                "foo = C.<caret>",
                fakeBrowseCache.get("Data.ByteString.Char8"));
        doTestInclude(
                "import qualified Data.ByteString.Char8\n" +
                "foo = Data.ByteString.Char8.<caret>",
                fakeBrowseCache.get("Data.ByteString.Char8"));
        // We should not autocomplete browse cache in imports.
        doTestExclude(
                "import Data.ByteString.Char8.<caret>",
                fakeBrowseCache.get("Data.ByteString.Char8"));
    }

    public void testLocalNames() throws Throwable {
        FakeBrowseCache fakeBrowseCache = new FakeBrowseCacheBuilder()
          .put("Prelude", "Just", "Nothing", "all", "any", "readFile")
          .put("Control.Monad", "liftM", "mapM", "forM")
          .put("C", "ByteString", "all", "any", "append", "appendFile", "break")
          .build();
        loadModuleSymbols(fakeBrowseCache);
        doTestInclude(
                "foo = <caret>",
                fakeBrowseCache.get("Prelude"));
        doTestInclude(
                "import Control.Monad\n" +
                "foo = <caret>",
                fakeBrowseCache.get("Control.Monad"));
        // Don't include Control.Monad completion if not imported.
        doTestExclude(
                "foo = <caret>",
                fakeBrowseCache.get("Control.Monad"));
        // Don't include all names when excluded from explicit import.
        doTestExclude(
                "import Control.Monad (liftM)\n" +
                "foo = <caret>",
                    "mapM", "forM");
        // Don't include names in the hiding clause.
        doTestExclude(
                "import Prelude hiding (Maybe(Just, Nothing), all)\n" +
                "foo = <caret>",
                    "Just", "Nothing", "all");
    }

    public void testHiddenNames() throws Throwable {
        FakeBrowseCache fakeBrowseCache = new FakeBrowseCacheBuilder()
          .put("Control.Monad", "liftM", "mapM", "forM")
          .build();
        loadModuleSymbols(fakeBrowseCache);
        String src =
                "import Control.Monad hiding (liftM)\n" +
                "main = <caret>";
        doTestExclude(src, "liftM");
        doTestInclude(src, "mapM", "forM");
    }

    public void testReferenceCompletion() throws Throwable {
        // Complete basic references.
        doTestInclude(
                "foo :: String -> String\n" +
                        "foo = undefined\n" +
                        "bar = <caret>",
                "foo");
        // Complete multiple names from a multi-name reference.
        doTestInclude(
                "foo, bar :: String -> String\n" +
                "foo = undefined\n" +
                "bar = <caret>",
                    "foo", "bar");
        // Don't complete references in the wrong context.
        doTestExclude(
                "import <caret>\n" +
                "foo :: String -> String\n" +
                "foo = undefined",
                    "foo");
        doTestExclude(
                "import qualified Control.Applicative as A\n" +
                "foo :: String -> String\n" +
                "foo = A.<caret>",
                    "foo");
    }

    public void testFunctionDeclCompletion() throws Throwable {
        // Complete the function name after its type signature.
        doTestInclude(
                "foo :: String -> String\n" +
                "<caret>",
                    "foo"
        );

        // Complete the function name after one of its definitions.
        doTestInclude(
                "foo = 1\n" +
                "<caret>",
                    "foo"
        );
    }

    private static class FakeBrowseCacheBuilder {

        private final FakeBrowseCache underlying = new FakeBrowseCache();

        FakeBrowseCacheBuilder put(String key, String... values) {
            underlying.put(key, values);
            return this;
        }

        public FakeBrowseCache build() {
            return underlying;
        }
    }

    private static class FakeBrowseCache extends HashMap<String, List<LookupElement>> {

        private final HashMap<String, String[]> nameMap = new HashMap<>();

        List<LookupElement> put(String key, String[] values) {
            List<LookupElement> result = super.put(key, ContainerUtil.map(values, LookupElementUtil::fromString));
            nameMap.put(key, values);
            return result;
        }

        public String[] get(String key) {
            return nameMap.get(key);
        }
    }
}
