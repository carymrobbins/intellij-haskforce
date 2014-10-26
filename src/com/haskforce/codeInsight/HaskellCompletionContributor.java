package com.haskforce.codeInsight;

import com.haskforce.HaskellIcons;
import com.haskforce.HaskellLanguage;
import com.haskforce.highlighting.annotation.external.GhcMod;
import com.haskforce.highlighting.annotation.external.GhcModi;
import com.haskforce.psi.*;
import com.haskforce.utils.LogicUtil;
import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.application.ApplicationManager;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.module.Module;
import com.intellij.openapi.module.ModuleUtilCore;
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.UserDataHolder;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.psi.PsiComment;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.Function;
import com.intellij.util.ProcessingContext;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * Fills the list of completions available on ctrl-space.
 */
public class HaskellCompletionContributor extends CompletionContributor {
    public static final Key<String[]> MODULE_CACHE_KEY = new Key("MODULE_CACHE");
    public static final Key<List<LookupElement>> LANGUAGE_CACHE_KEY = new Key("LANGUAGE_CACHE");
    public static final Key<String[]> FLAG_CACHE_KEY = new Key("FLAG_CACHE");
    public static final Key<Map<String, List<LookupElement>>> BROWSE_CACHE_KEY = new Key("BROWSE_CACHE");

    private static String[] PRAGMA_TYPES = new String[]{
            "LANGUAGE ", "OPTIONS_GHC ", "WARNING ", "DEPRECATED ", "INLINE ", "NOINLINE ", "INLINABLE ", "CONLIKE ",
            "RULES ", "ANN ", "LINE ", "SPECIALIZE ", "UNPACK ", "SOURCE "};

    public static String[] getPragmaTypes() {
        return PRAGMA_TYPES.clone();
    }

    public HaskellCompletionContributor() {
        // TODO: It probably makes more sense to use a single extend() to more easily control including completions
        // so we can share traversals of the Psi tree and integrate into logic paths.  However, it would probably
        // be best at that point to break out sections into methods to avoid creating one giant function.

        // Keywords
        // TODO: Only 'import' is implemented; implement other keywords with context-awareness.
        extend(CompletionType.BASIC,
                PlatformPatterns.psiElement().withLanguage(HaskellLanguage.INSTANCE),
                new CompletionProvider<CompletionParameters>() {
                    public void addCompletions(@NotNull CompletionParameters parameters,
                                               ProcessingContext context,
                                               @NotNull CompletionResultSet result) {
                        // Traverse upwards, stop before we reach the top-level body.
                        PsiElement el = parameters.getPosition();
                        while (el != null) {
                            PsiElement parent = el.getParent();
                            if (parent instanceof HaskellBody) {
                                break;
                            }
                            el = parent;
                        }
                        PsiElement prev = getPrevSiblingWhere(new Function<PsiElement, Boolean>() {
                            @Override
                            public Boolean fun(PsiElement psiElement) {
                                return !PsiTreeUtil.instanceOf(psiElement,
                                        HaskellPpragma.class, PsiWhiteSpace.class, PsiComment.class);
                            }
                        }, el);
                        // Check if previous sibling is an import or does not exist (first import).
                        if (prev == null || prev instanceof HaskellImpdecl) {
                            result.addElement(stringToLookupElement.fun("import "));
                        }
                    }
                }
        );

        // Autocomplete "qualified" after "import"
        extend(CompletionType.BASIC,
                PlatformPatterns.psiElement().afterLeaf("import").withLanguage(HaskellLanguage.INSTANCE),
                new CompletionProvider<CompletionParameters>() {
                    public void addCompletions(@NotNull CompletionParameters parameters,
                                               ProcessingContext context,
                                               @NotNull CompletionResultSet result) {
                        result.addElement(stringToLookupElement.fun("qualified "));
                    }
                }
        );

        // Pragma
        extend(CompletionType.BASIC,
                PlatformPatterns.psiElement(HaskellTypes.PRAGMA).withLanguage(HaskellLanguage.INSTANCE),
                new CompletionProvider<CompletionParameters>() {
                    @Override
                    protected void addCompletions(@NotNull CompletionParameters parameters,
                                                  ProcessingContext context,
                                                  @NotNull CompletionResultSet result) {
                        final PsiElement position = parameters.getPosition();
                        final PsiElement prevSibling = getPrevSiblingWhere(new Function<PsiElement, Boolean>() {
                            @Override
                            public Boolean fun(PsiElement psiElement) {
                                return !(psiElement instanceof PsiWhiteSpace);
                            }
                        }, position);

                        // Pragma types.
                        if (prevSibling != null && "{-#".equals(prevSibling.getText())) {
                            addAllElements(result, LogicUtil.map(stringToLookupElement, PRAGMA_TYPES));
                        }

                        final PsiElement openPragma = getPrevSiblingWhere(new Function<PsiElement, Boolean>() {
                            @Override
                            public Boolean fun(PsiElement psiElement) {
                                return psiElement.getText().equals("{-#");
                            }
                        }, position);

                        final PsiElement pragmaTypeElement = getNextSiblingWhere(new Function<PsiElement, Boolean>() {
                            @Override
                            public Boolean fun(PsiElement psiElement) {
                                return !(psiElement instanceof PsiWhiteSpace);
                            }
                        }, openPragma);

                        if (pragmaTypeElement == null) {
                            return;
                        }

                        final String pragmaType = pragmaTypeElement.getText();

                        final PsiFile originalFile = parameters.getOriginalFile();
                        final UserDataHolder cacheHolder = getCacheHolder(originalFile);
                        if ("LANGUAGE".equals(pragmaType)) {
                            addAllElements(result, cacheHolder.getUserData(LANGUAGE_CACHE_KEY));
                        } else if ("OPTIONS_GHC".equals(pragmaType)) {
                            // TODO: Workaround since completion autocompletes after the "-", so without this
                            // we may end up completing -foo with --foo (inserting a "-").
                            final String[] flags = cacheHolder.getUserData(FLAG_CACHE_KEY);
                            if (position.getText().startsWith("-")) {
                                addAllElements(result, LogicUtil.map(new Function<String, LookupElement>() {
                                    @Override
                                    public LookupElement fun(String s) {
                                        return stringToLookupElement.fun(s.startsWith("-") ? s.substring(1) : s);
                                    }
                                }, flags));
                            } else {
                                addAllElements(result, LogicUtil.map(stringToLookupElement, flags));
                            }
                        }
                    }
                });

        // Modules
        extend(CompletionType.BASIC,
                PlatformPatterns.psiElement().withLanguage(HaskellLanguage.INSTANCE),
                new CompletionProvider<CompletionParameters>() {
                    @Override
                    protected void addCompletions(@NotNull CompletionParameters parameters,
                                                  ProcessingContext context,
                                                  @NotNull CompletionResultSet result) {
                        // TODO: Refactor this implementation.
                        final PsiElement position = parameters.getPosition();
                        PsiElement el = position.getParent();
                        if (!(el instanceof HaskellConid)) {
                            return;
                        }
                        el = el.getParent();
                        if (!(el instanceof HaskellQconid)) {
                            return;
                        }
                        el = el.getParent();
                        if (!(el instanceof HaskellImpdecl)) {
                            return;
                        }
                        final String[] list = getCacheHolder(parameters.getOriginalFile()).getUserData(MODULE_CACHE_KEY);
                        if (list == null) {
                            return;
                        }
                        StringBuilder builder = new StringBuilder(0);
                        el = position.getParent();
                        while (el != null) {
                            el = el.getPrevSibling();
                            if (el != null) {
                                builder.insert(0, el.getText());
                            }
                        }
                        final String partialModule = builder.toString();
                        Set<String> newLines = new HashSet<String>(0);
                        for (String line : list) {
                            if (line.startsWith(partialModule)) {
                                String newLine = line.replace(partialModule, "");
                                final int firstDotPos = newLine.indexOf('.');
                                if (firstDotPos != -1) {
                                    newLine = newLine.substring(0, firstDotPos);
                                }
                                newLines.add(newLine);
                            }
                        }
                        addAllElements(result, LogicUtil.map(stringToLookupElement, newLines));
                    }
                });

        // Importing names.
        extend(CompletionType.BASIC,
                PlatformPatterns.psiElement().withLanguage(HaskellLanguage.INSTANCE),
                new CompletionProvider<CompletionParameters>() {
                    @Override
                    protected void addCompletions(@NotNull CompletionParameters parameters, ProcessingContext context, @NotNull CompletionResultSet result) {
                        final PsiElement position = parameters.getPosition();
                        PsiElement el = position.getParent();
                        if (el == null) {
                            return;
                        }
                        el = el.getParent();
                        if (!(el instanceof HaskellImportt)) {
                            if (el != null) {
                                el = el.getParent();
                                if (!(el instanceof HaskellImportt)) {
                                    return;
                                }
                            }
                        }
                        el = getPrevSiblingWhere(new Function<PsiElement, Boolean>() {
                            @Override
                            public Boolean fun(PsiElement psiElement) {
                                return psiElement instanceof HaskellQconid;
                            }
                        }, el);
                        if (el == null) {
                            return;
                        }
                        final String module = el.getText();
                        final PsiFile file = parameters.getOriginalFile();
                        final UserDataHolder cacheHolder = getCacheHolder(file);
                        final Map<String, List<LookupElement>> cachedNames = cacheHolder.getUserData(BROWSE_CACHE_KEY);
                        if (cachedNames != null) {
                            addAllElements(result, cachedNames.get(module));
                        }
                    }
                });

        // Qualified names.
        extend(CompletionType.BASIC,
                PlatformPatterns.psiElement().withLanguage(HaskellLanguage.INSTANCE),
                new CompletionProvider<CompletionParameters>() {
                    @Override
                    protected void addCompletions(@NotNull CompletionParameters parameters, ProcessingContext context, @NotNull CompletionResultSet result) {
                        final PsiElement position = parameters.getPosition();
                        PsiElement el = position.getParent();
                        if (el == null) {
                            return;
                        }
                        el = el.getParent();
                        if (!(el instanceof HaskellQconid || el instanceof HaskellQvarid)) {
                            return;
                        }
                        final String qName = el.getText();
                        final int lastDotPos = qName.lastIndexOf('.');
                        if (lastDotPos == -1) {
                            return;
                        }
                        final String module = qName.substring(0, lastDotPos);
                        // Pull user-qualified names from cache.
                        final PsiFile file = parameters.getOriginalFile();
                        final UserDataHolder cacheHolder = getCacheHolder(file);
                        final Map<String, List<LookupElement>> cachedNames = cacheHolder.getUserData(BROWSE_CACHE_KEY);
                        if (cachedNames != null) {
                            addAllElements(result, cachedNames.get(module));
                        }
                    }
                });

        // Local names.
        extend(CompletionType.BASIC,
                PlatformPatterns.psiElement().withLanguage(HaskellLanguage.INSTANCE),
                new CompletionProvider<CompletionParameters>() {
                    @Override
                    protected void addCompletions(@NotNull CompletionParameters parameters, ProcessingContext context, @NotNull CompletionResultSet result) {
                        final PsiElement position = parameters.getPosition();
                        if (PsiTreeUtil.getParentOfType(position, HaskellExp.class) == null) {
                            return;
                        }
                        final PsiFile file = parameters.getOriginalFile();
                        final UserDataHolder cacheHolder = getCacheHolder(file);
                        final Map<String, List<LookupElement>> cachedNames = cacheHolder.getUserData(BROWSE_CACHE_KEY);
                        if (cachedNames == null) {
                            return;
                        }
                        final Map<String, String> aliasMap = mapAliasesToModules(file);
                        for (Map.Entry<String, String> e : aliasMap.entrySet()) {
                            final String alias = e.getKey();
                            final String module = e.getValue();
                            // If they are equal, then the module probably wasn't imported qualified.
                            // Although, this won't work when it's fully qualified, e.g. import qualified Foo.Bar
                            if (alias.equals(module)) {
                                addAllElements(result, cachedNames.get(module));
                            }
                        }
                    }
                });
    }

    /**
     * Returns a map of alias -> module for each imported module.  For modules imported without an alias, the
     * full module name itself will be considered the alias.
     */
    public static Map<String, String> mapAliasesToModules(@NotNull final PsiFile file) {
        PsiElement el = null;
        for (PsiElement child : file.getChildren()) {
            if (child instanceof HaskellBody) {
                el = child;
                break;
            }
        }
        if (el == null) {
            return null;
        }
        Map<String, String> result = new HashMap(1);
        // For simplicity, let's just assume Prelude is always there.
        result.put("Prelude", "Prelude");
        for (PsiElement child : el.getChildren()) {
            if (child instanceof HaskellImpdecl) {
                String module = null;
                String alias = null;
                for (PsiElement gChild : child.getChildren()) {
                    if (module == null && gChild instanceof HaskellQconid) {
                        module = gChild.getText();
                    } else if (module != null && gChild instanceof HaskellQconid) {
                        alias = gChild.getText();
                    }
                }
                if (module != null) {
                    // If the user didn't alias the module then the full module name is used.
                    if (alias == null) {
                        alias = module;
                    }
                    result.put(alias, module);
                }

            }
        }
        return result;
    }

    @Nullable
    public static Map<String, List<LookupElement>> getQualifiedNameCache(@NotNull final UserDataHolder holder,
                                                                         @NotNull final PsiFile file,
                                                                         @NotNull final Project project,
                                                                         @NotNull final String workDir,
                                                                         final boolean force) {
        GhcModi ghcModi = GhcModi.getInstance(project, workDir);
        if (ghcModi == null) {
            return null;
        }
        Map<String, List<LookupElement>> browseCache = force ? null : holder.getUserData(BROWSE_CACHE_KEY);
        Map<String, String> aliasMap = mapAliasesToModules(file);
        if (browseCache == null) {
            browseCache = new HashMap<String, List<LookupElement>>(aliasMap.size());
        }
        for (Map.Entry<String, String> e : aliasMap.entrySet()) {
            final String alias = e.getKey();
            final String module = e.getValue();
            final List<LookupElement> cachedNames = browseCache.get(module);
            if (cachedNames != null) {
                browseCache.put(alias, cachedNames);
                continue;
            }
            final List<LookupElement> names = LogicUtil.map(browseItemToLookupElement, ghcModi.browse(module));
            // We should autocomplete for the alias and the fully qualified module name.
            browseCache.put(alias, names);
            browseCache.put(module, names);
        }
        return browseCache;
    }

    /**
     * Helper method to load data from ghc-mod into a file cache to be used for autocompletion.  This is done in a
     * Java thread so execution can continue.  It just so happens to be very convenient to do this in the external
     * annotator; however, there may be a better place to do this.
     */
    public static void loadCacheData(@NotNull final PsiFile file, @NotNull final Project project, @NotNull final String workDir) {
        loadCacheData(file, project, workDir, false);
    }

    public static void loadCacheData(@NotNull final PsiFile file, @NotNull final Project project, @NotNull final String workDir, final boolean force) {
        final UserDataHolder cache = getCacheHolder(file);
        ApplicationManager.getApplication().invokeLater(new Runnable() {
            @Override
            public void run() {
                if (force || cache.getUserData(LANGUAGE_CACHE_KEY) == null) {
                    cache.putUserData(LANGUAGE_CACHE_KEY, LogicUtil.map(stringToLookupElement, GhcMod.lang(project, workDir)));
                }
                if (force || cache.getUserData(FLAG_CACHE_KEY) == null) {
                    cache.putUserData(FLAG_CACHE_KEY, GhcMod.flag(project, workDir));
                }
                if (force || cache.getUserData(MODULE_CACHE_KEY) == null) {
                    cache.putUserData(MODULE_CACHE_KEY, GhcMod.list(project, workDir));
                }
                // Checks for existing cache are done in getQualifiedNameCache()
                cache.putUserData(BROWSE_CACHE_KEY, getQualifiedNameCache(cache, file, project, workDir, force));
            }
        });
    }

    public static UserDataHolder getCacheHolder(@NotNull PsiFile file) {
        final Module module = ModuleUtilCore.findModuleForPsiElement(file);
        return module == null ? file : module;
    }

    /**
     * Helper to prevent having to do a null check before adding elements to the completion result.
     */
    public static void addAllElements(CompletionResultSet result, List<LookupElement> elements) {
        if (elements != null) {
            result.addAllElements(elements);
        }
    }

    @Nullable
    public static PsiElement getFirstElementWhere(Function<PsiElement, PsiElement> modify,
                                                  Function<PsiElement, Boolean> where,
                                                  PsiElement initialElement) {
        if (initialElement == null) {
            return null;
        }
        PsiElement result = modify.fun(initialElement);
        while (result != null) {
            if (where.fun(result)) {
                return result;
            }
            result = modify.fun(result);
        }
        return null;
    }

    @Nullable
    public static PsiElement getPrevSiblingWhere(Function<PsiElement, Boolean> f, PsiElement e) {
        return getFirstElementWhere(new Function<PsiElement, PsiElement>() {
            @Override
            public PsiElement fun(PsiElement psiElement) {
                return psiElement.getPrevSibling();
            }
        }, f, e);
    }

    @Nullable
    public static PsiElement getNextSiblingWhere(Function<PsiElement, Boolean> f, PsiElement e) {
        return getFirstElementWhere(new Function<PsiElement, PsiElement>() {
            @Override
            public PsiElement fun(PsiElement psiElement) {
                return psiElement.getNextSibling();
            }
        }, f, e);
    }

    /**
     * Adjust the error message when no lookup is found.
     */
    @Nullable
    @Override
    public String handleEmptyLookup(@NotNull CompletionParameters parameters, final Editor editor) {
        return "HaskForce: no completion found.";
    }

    public static LookupElement createLookupElement(@NotNull String name, @NotNull String module, @NotNull String type) {
        return LookupElementBuilder.create(name).withIcon(HaskellIcons.FILE)
                .withTailText(" (" + module + ')', true)
                .withTypeText(type);
    }

    public static final Function<String, LookupElement> stringToLookupElement = new Function<String, LookupElement>() {
        @Override
        public LookupElement fun(String s) {
            return LookupElementBuilder.create(s).withIcon(HaskellIcons.FILE);
        }
    };

    public static final Function<GhcModi.BrowseItem, LookupElement> browseItemToLookupElement = new Function<GhcModi.BrowseItem, LookupElement>() {
        @Override
        public LookupElement fun(GhcModi.BrowseItem x) {
            return createLookupElement(x.name, x.module, x.type);
        }
    };
}
