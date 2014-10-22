package com.haskforce.codeInsight;

import com.haskforce.psi.impl.HaskellElementFactory;
import com.haskforce.utils.LogicUtil;
import static com.haskforce.codeInsight.HaskellCompletionContributor.*;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.psi.PsiFile;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * Completion test driver. Add new completion testcases here.
 */
public class HaskellCompletionTest extends HaskellCompletionTestBase {
    public void testKeywordImport() throws Throwable {
        doTestInclude("<caret>", "import ");
    }

    public void testKeywordQualified() throws Throwable {
        doTestInclude("import <caret>", "qualified ");
    }

    public void testPragmaTypes() throws Throwable {
        doTestInclude("{-# <caret> #-}",  PRAGMA_TYPES);
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
                "import Data.Maybe");
        Map<String, String> map = mapAliasesToModules(file);
        assertEquals(map.get("C"), "Data.ByteString.Char8");
        assertEquals(map.get("Data.Maybe"), "Data.Maybe");
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
                "import qualified Data.ByteString.Char8 as C\n" +
                "foo = Data.ByteString.Char8.<caret>",
                fakeBrowseCache.get("Data.ByteString.Char8"));
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
