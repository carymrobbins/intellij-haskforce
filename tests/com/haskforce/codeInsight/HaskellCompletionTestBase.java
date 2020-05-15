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

package com.haskforce.codeInsight;

// Imported from Erlang repository on 24 July 2014.

import com.haskforce.HaskellFileType;
import com.haskforce.HaskellLightPlatformCodeInsightFixtureTestCase;
import com.haskforce.codeInsight.HaskellCompletionCacheLoader.LookupElementWrapper;
import com.haskforce.codeInsight.HaskellCompletionCacheLoader.LookupElementWrapper$;
import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.openapi.fileTypes.FileType;
import com.intellij.psi.PsiFile;
import com.intellij.testFramework.UsefulTestCase;
import com.intellij.util.Function;
import com.intellij.util.containers.HashSet;

import java.util.*;

/**
 * Common functionality for completion tests
 */
abstract public class HaskellCompletionTestBase extends HaskellLightPlatformCodeInsightFixtureTestCase {
    final protected List<Function<HaskellCompletionCacheLoader.Cache, Void>> cacheLoaders;

    protected HaskellCompletionTestBase(String srcName) {
        super(srcName, srcName);
        cacheLoaders = new ArrayList<Function<HaskellCompletionCacheLoader.Cache, Void>>(0);
    }

    protected HaskellCompletionTestBase() {
        this("codeInsight");
    }

/*
    protected void localFileSystemSetUp() throws Exception {
        IdeaTestFixtureFactory factory = IdeaTestFixtureFactory.getFixtureFactory();
        TestFixtureBuilder<IdeaProjectTestFixture> fixtureBuilder = factory.createLightFixtureBuilder(getProjectDescriptor());

        final IdeaProjectTestFixture fixture = fixtureBuilder.getFixture();
        myFixture = IdeaTestFixtureFactory.getFixtureFactory().createCodeInsightFixture(fixture, new TempDirTestFixtureImpl());

        InjectedLanguageManagerImpl.checkInjectorsAreDisposed(getProject());
        myFixture.setUp();
        myFixture.setTestDataPath(getTestDataPath());
        myModule = myFixture.getModule();
    }

    protected void doCheckResult(@NotNull String before, @NotNull String after) { doCheckResult(before, after, null); }

    protected void doCheckResult(@NotNull String before, @NotNull String after, @Nullable Character c) {
        myFixture.configureByText("a.erl", before);
        myFixture.completeBasic();
        if (c != null) myFixture.type(c);
        myFixture.checkResult(after);
    }

      protected void doSmartTest(String text, CheckType type, String... variants) throws Throwable { doTestVariants(text, CompletionType.SMART, 1, type, variants); }

      protected void doTestEquals(String txt, String... variants) throws Throwable {
        doTestVariants(txt, CompletionType.BASIC, 1, CheckType.EQUALS, variants);
      }
*/

    protected void doTestEqual(String txt, String... variants) throws Throwable {
        doTestVariants(txt, CompletionType.BASIC, 1, CheckType.EQUALS, variants);
    }

    protected void doTestInclude(String txt, String... variants) throws Throwable {
        doTestVariants(txt, CompletionType.BASIC, 1, CheckType.INCLUDES, variants);
    }

    protected void doTestExclude(String txt, String... variants) throws Throwable {
        doTestVariants(txt, CompletionType.BASIC, 1, CheckType.EXCLUDES, variants);
    }

    protected FileType getFileType() {
        return HaskellFileType.INSTANCE;
    }

    protected void doTestVariants(String txt, CompletionType type, int count,
                                  CheckType checkType,
                                  String... variants) throws Throwable {
        myFixture.configureByText(getFileType(), txt);
        PsiFile file = myFixture.getFile();
        HaskellCompletionCacheLoader.Cache cacheHolder = HaskellCompletionCacheLoader.getService(file.getProject()).cache();
        for (Function<HaskellCompletionCacheLoader.Cache, Void> f : cacheLoaders) {
            f.fun(cacheHolder);
        }
        doTestVariantsInner(type, count, checkType, variants);
    }

    protected void loadLanguageExtensions(final String... ss) {
        cacheLoaders.add(new Function<HaskellCompletionCacheLoader.Cache, Void>() {
            @Override
            public Void fun(HaskellCompletionCacheLoader.Cache cache) {
                for (String s : ss) {
                    cache.languageExtensions().add(LookupElementWrapper$.MODULE$.fromString(s));
                }
                return null;
            }
        });
    }

    protected void loadGhcFlags(final String... ss) {
        cacheLoaders.add(new Function<HaskellCompletionCacheLoader.Cache, Void>() {
            @Override
            public Void fun(HaskellCompletionCacheLoader.Cache cache) {
                Collections.addAll(cache.ghcFlags(), ss);
                return null;
            }
        });
    }

    protected void loadVisibleModules(final String... ss) {
        cacheLoaders.add(new Function<HaskellCompletionCacheLoader.Cache, Void>() {
            @Override
            public Void fun(HaskellCompletionCacheLoader.Cache cache) {
                Collections.addAll(cache.visibleModules(), ss);
                return null;
            }
        });
    }

    protected void loadModuleSymbols(final Map<String, List<LookupElement>> m) {
        cacheLoaders.add(new Function<HaskellCompletionCacheLoader.Cache, Void>() {
            @Override
            public Void fun(HaskellCompletionCacheLoader.Cache cache) {
                Map<String, Set<LookupElementWrapper>> syms = cache.moduleSymbols();
                for (Map.Entry<String, List<LookupElement>> kvp : m.entrySet()) {
                    Set<LookupElementWrapper> v = syms.get(kvp.getKey());
                    if (v == null) {
                        v = new HashSet<LookupElementWrapper>(kvp.getValue().size());
                        syms.put(kvp.getKey(), v);
                    }
                    for (LookupElement e : kvp.getValue()) {
                        v.add(new LookupElementWrapper(e));
                    }
                }
                return null;
            }
        });
    }

    protected void clearCache() {
        cacheLoaders.clear();
    }

    protected void doTestVariantsInner(CompletionType type, int count,
                                       CheckType checkType,
                                       String... variants) throws Throwable {
        myFixture.complete(type, count);
        List<String> stringList = myFixture.getLookupElementStrings();

        assertNotNull("\nPossibly the single variant has been completed.\n" +
                        "File after:\n" +
                        myFixture.getFile().getText(),
                stringList);
        Collection<String> varList = new ArrayList<String>(Arrays.asList(variants));
        if (checkType == CheckType.EQUALS) {
            UsefulTestCase.assertSameElements(stringList, variants);
        }
        else if (checkType == CheckType.INCLUDES) {
            varList.removeAll(stringList);
            assertTrue("Missing variants: " + varList, varList.isEmpty());
        }
        else if (checkType == CheckType.EXCLUDES) {
            varList.retainAll(stringList);
            assertTrue("Unexpected variants: " + varList, varList.isEmpty());
        }
    }

    public enum CheckType { EQUALS, INCLUDES, EXCLUDES }
}
