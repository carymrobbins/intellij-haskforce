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

import java.util.*;
import java.util.function.Consumer;

/**
 * Common functionality for completion tests
 */
abstract public class HaskellCompletionTestBase extends HaskellLightPlatformCodeInsightFixtureTestCase {
    final protected List<Consumer<HaskellCompletionCacheLoader.Cache>> cacheLoaders;

    protected HaskellCompletionTestBase(String srcName) {
        super(srcName, srcName);
        cacheLoaders = new ArrayList<>(0);
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

    protected void doTestEqual(String txt, String... variants) {
        doTestVariants(txt, CheckType.EQUALS, variants);
    }

    protected void doTestInclude(String txt, String... variants) {
        doTestVariants(txt, CheckType.INCLUDES, variants);
    }

    protected void doTestExclude(String txt, String... variants) {
        doTestVariants(txt, CheckType.EXCLUDES, variants);
    }

    protected FileType getFileType() {
        return HaskellFileType.INSTANCE;
    }

    protected void doTestVariants(
      String txt,
      CheckType checkType,
      String... variants
    ) {
        myFixture.configureByText(getFileType(), txt);
        PsiFile file = myFixture.getFile();
        HaskellCompletionCacheLoader.Cache cacheHolder = HaskellCompletionCacheLoader.getService(file.getProject()).cache();
        cacheLoaders.forEach(f -> f.accept(cacheHolder));
        doTestVariantsInner(CompletionType.BASIC, 1, checkType, variants);
    }

    protected void loadLanguageExtensions(final String... ss) {
        cacheLoaders.add(cache ->
            Arrays.stream(ss).forEach(s ->
                cache.languageExtensions().add(LookupElementWrapper$.MODULE$.fromString(s))
            )
        );
    }

    protected void loadGhcFlags(final String... ss) {
        cacheLoaders.add(cache ->
            Collections.addAll(cache.ghcFlags(), ss)
        );
    }

    protected void loadVisibleModules(final String... ss) {
        cacheLoaders.add(cache -> {
            String filePath = myFixture.getFile().getVirtualFile().getCanonicalPath();
            assertNotNull(filePath);
            cache.visibleModulesByFile().put(filePath, ss);
        });
    }

    protected void loadModuleSymbols(final Map<String, List<LookupElement>> m) {
        cacheLoaders.add(cache -> {
            Map<String, Set<LookupElementWrapper>> syms = cache.moduleSymbols();
            for (Map.Entry<String, List<LookupElement>> kvp : m.entrySet()) {
                Set<LookupElementWrapper> v = syms.computeIfAbsent(
                  kvp.getKey(),
                  k -> new HashSet<>(kvp.getValue().size())
                );
                for (LookupElement e : kvp.getValue()) {
                    v.add(new LookupElementWrapper(e));
                }
            }
        });
    }

    protected void clearCache() {
        cacheLoaders.clear();
    }

    protected void doTestVariantsInner(CompletionType type, int count,
                                       CheckType checkType,
                                       String... variants) {
        myFixture.complete(type, count);
        List<String> stringList = myFixture.getLookupElementStrings();

        assertNotNull("\nPossibly the single variant has been completed.\n" +
                        "File after:\n" +
                        myFixture.getFile().getText(),
                stringList);
        Collection<String> varList = new ArrayList<>(Arrays.asList(variants));
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
