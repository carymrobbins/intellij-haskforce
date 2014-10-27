package com.haskforce.codeInsight;

import com.haskforce.psi.impl.HaskellElementFactory;
import com.haskforce.utils.LogicUtil;
import static com.haskforce.codeInsight.HaskellCompletionContributor.*;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.openapi.util.Pair;
import com.intellij.psi.PsiFile;
import com.intellij.util.Function;

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
                "{-# ANN module \"foo\" -#}\n" +
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
    }

    public void testKeywordQualified() throws Throwable {
        doTestInclude("import <caret>", "qualified ");
    }

    public void testPragmaTypes() throws Throwable {
        doTestInclude("{-# <caret> #-}",  getPragmaTypes());
    }

    public void testLanguages() throws Throwable {
        final String[] fakeLangs = {"OverloadedStrings", "TypeFamilies", "OverloadedRecordFields"};
        loadCache(LANGUAGE_CACHE_KEY, fakeLangs);
        doTestInclude("{-# LANGUAGE <caret> #-}", fakeLangs);
    }

    public void testGhcFlags() throws Throwable {
        final String[] fakeFlags = {"-ferror-spans", "-fno-error-spans", "-fprint-explicit-foralls"};
        loadCache(FLAG_CACHE_KEY, fakeFlags);
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
        loadCache(MODULE_CACHE_KEY, fakeModules);
        doTestInclude("import <caret>", "Control", "Data");
        doTestInclude("import Data.<caret>", "ByteString", "Byteable");
        doTestExclude("import Data.<caret>", "import ");
    }

    public void testNameImports() throws Throwable {
        FakeBrowseCache fakeBrowseCache = new FakeBrowseCache(
                "Data.Ord", Arrays.asList("Down", "EQ", "GT", "LT", "Ord", "Ordering", "compare", "comparing"));
        loadCache(BROWSE_CACHE_KEY, fakeBrowseCache);
        doTestInclude("import Data.Ord (<caret>)", fakeBrowseCache.get("Data.Ord"));
        doTestInclude("import Data.Ord (c<caret>)", "compare", "comparing");
        doTestInclude("import Data.Ord (Or<caret>)", "Ord", "Ordering");
    }

    public void testImportAliasMapping() throws Throwable {
        PsiFile file = HaskellElementFactory.createFileFromText(myFixture.getProject(),
                "import qualified Data.ByteString.Char8 as C\n" +
                "import qualified Control.Monad\n" +
                "import Data.Maybe");
        assertContainsElements(parseModuleAliases(file),
                new ModuleAlias("Data.ByteString.Char8", "C"),
                new ModuleAlias("Control.Monad", "Control.Monad"),
                new ModuleAlias("Data.Maybe", null));
    }

    public void testQualifiedNames() throws Throwable {
        FakeBrowseCache fakeBrowseCache = new FakeBrowseCache(
                "Data.ByteString.Char8", Arrays.asList("ByteString", "all", "any", "append", "appendFile", "break"),
                "C", Arrays.asList("ByteString", "all", "any", "append", "appendFile", "break"));
        loadCache(BROWSE_CACHE_KEY, fakeBrowseCache);
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
        FakeBrowseCache fakeBrowseCache = new FakeBrowseCache(
                "Prelude", Arrays.asList("Just", "Nothing", "all", "any", "readFile"),
                "Control.Monad", Arrays.asList("liftM", "mapM", "forM"),
                "C", Arrays.asList("ByteString", "all", "any", "append", "appendFile", "break"));
        loadCache(BROWSE_CACHE_KEY, fakeBrowseCache);
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

    /**
     * Terrible hack to make constructing and using these maps way simpler.
     */
    static class FakeBrowseCache extends HashMap<String, List<LookupElement>> {
        private HashMap<String, String[]> nameMap;

        FakeBrowseCache(Object... objects) {
            super(objects.length / 2);
            nameMap = new HashMap(objects.length / 2);
            String key = null;
            for (int i = 0; i < objects.length; ++i) {
                if (i % 2 == 0) {
                    key = (String)objects[i];
                } else {
                    put(key, (List<String>)objects[i]);
                }
            }
        }

        public List<LookupElement> put(String key, List<String> value) {
            List<LookupElement> result = super.put(key, LogicUtil.map(stringToLookupElement, value));
            //noinspection SuspiciousArrayCast
            nameMap.put(key, (String[])(value.toArray()));
            return result;
        }

        public String[] get(String key) {
            return nameMap.get(key);
        }
    }
}
