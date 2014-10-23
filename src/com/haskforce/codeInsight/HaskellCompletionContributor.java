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
import com.intellij.openapi.project.Project;
import com.intellij.openapi.util.Key;
import com.intellij.openapi.util.Pair;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.psi.PsiElement;
import com.intellij.psi.PsiFile;
import com.intellij.psi.PsiWhiteSpace;
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
        extend(CompletionType.BASIC,
                PlatformPatterns.psiElement().withLanguage(HaskellLanguage.INSTANCE),
                new CompletionProvider<CompletionParameters>() {
                    public void addCompletions(@NotNull CompletionParameters parameters,
                                               ProcessingContext context,
                                               @NotNull CompletionResultSet result) {
                        // We don't really need every keyword, just the longer ones (probably).
                        // It's also helpful to add the trailing space (if applicable) so the user doesn't have to.
                        // TODO: Refine these keywords within their appropriate scope in the psi tree.
                        addAllElements(result, LogicUtil.map(stringToLookupElement, new String[]{
                                "deriving ",
                                "import ",
                                "instance",
                                "module ",
                                "newtype ",
                                "where",
                        }));
                    }
                }
        );

        // Only autocomplete "qualified" after "import"
        extend(CompletionType.BASIC,
                PlatformPatterns.psiElement().afterLeaf("import").withLanguage(HaskellLanguage.INSTANCE),
                new CompletionProvider<CompletionParameters>() {
                    public void addCompletions(@NotNull CompletionParameters parameters,
                                               ProcessingContext context,
                                               @NotNull CompletionResultSet result) {
                        result.addElement(LookupElementBuilder.create("qualified "));
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
                        final PsiFile originalFile = parameters.getOriginalFile();

                        final PsiElement prevSibling = getFirstPrevSiblingWhere(new Function<PsiElement, Boolean>() {
                            @Override
                            public Boolean fun(PsiElement psiElement) {
                                return !(psiElement instanceof PsiWhiteSpace);
                            }
                        }, position);

                        // Pragma types.
                        if (prevSibling != null && "{-#".equals(prevSibling.getText())) {
                            addAllElements(result, LogicUtil.map(stringToLookupElement, PRAGMA_TYPES));
                        }

                        final PsiElement openPragma = getFirstPrevSiblingWhere(new Function<PsiElement, Boolean>() {
                            @Override
                            public Boolean fun(PsiElement psiElement) {
                                return psiElement.getText().equals("{-#");
                            }
                        }, position);

                        final PsiElement pragmaTypeElement = getFirstNextSiblingWhere(new Function<PsiElement, Boolean>() {
                            @Override
                            public Boolean fun(PsiElement psiElement) {
                                return !(psiElement instanceof PsiWhiteSpace);
                            }
                        }, openPragma);

                        if (pragmaTypeElement == null) {
                            return;
                        }

                        final String pragmaType = pragmaTypeElement.getText();

                        if ("LANGUAGE".equals(pragmaType)) {
                            addAllElements(result, originalFile.getUserData(LANGUAGE_CACHE_KEY));
                        } else if ("OPTIONS_GHC".equals(pragmaType)) {
                            // TODO: Workaround since completion autocompletes after the "-", so without this
                            // we may end up completing -foo with --foo (inserting a "-").
                            final String[] flags = originalFile.getUserData(FLAG_CACHE_KEY);
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
                        // TODO: Write some tests!
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
                        final String[] list = parameters.getOriginalFile().getUserData(MODULE_CACHE_KEY);
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
                        final PsiFile file = parameters.getOriginalFile();
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
                        el = getFirstPrevSiblingWhere(new Function<PsiElement, Boolean>() {
                            @Override
                            public Boolean fun(PsiElement psiElement) {
                                return psiElement instanceof HaskellQconid;
                            }
                        }, el);
                        if (el == null) {
                            return;
                        }
                        final String module = el.getText();
                        final Map<String, List<LookupElement>> cache = file.getUserData(BROWSE_CACHE_KEY);
                        if (cache != null) {
                            addAllElements(result, cache.get(module));
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
                        final PsiFile file = parameters.getOriginalFile();
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
                        final Map<String, List<LookupElement>> cache = file.getUserData(BROWSE_CACHE_KEY);
                        if (cache != null) {
                            addAllElements(result, cache.get(module));
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
        Map<String, String> result = new HashMap(0);
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

    @NotNull
    public static Map<String, List<LookupElement>> getQualifiedNameCache(@NotNull final PsiFile file,
                                                                         @NotNull final Project project,
                                                                         @NotNull final String workDir) {
        Map<String, List<LookupElement>> result = new HashMap(0);
        GhcModi ghcModi = GhcModi.getInstance(project, workDir);
        if (ghcModi == null) {
            return result;
        }
        for (Map.Entry<String, String> e : mapAliasesToModules(file).entrySet()) {
            final String alias = e.getKey();
            final String module = e.getValue();
            final List<LookupElement> names = LogicUtil.map(pairToLookupElement, ghcModi.browse(module));
            // We should autocomplete for the alias and the fully qualified module name.
            result.put(alias, names);
            result.put(module, names);
        }
        return result;
    }

    /**
     * Helper method to load data from ghc-mod into a file cache to be used for autocompletion.  This is done in a
     * Java thread so execution can continue.  It just so happens to be very convenient to do this in the external
     * annotator; however, there may be a better place to do this.
     */
    public static void loadCacheData(@NotNull final PsiFile file, @NotNull final Project project, @NotNull final String workDir) {
        ApplicationManager.getApplication().invokeLater(new Runnable() {
            @Override
            public void run() {
                file.putUserData(LANGUAGE_CACHE_KEY, LogicUtil.map(stringToLookupElement, GhcMod.lang(project, workDir)));
                file.putUserData(FLAG_CACHE_KEY, GhcMod.flag(project, workDir));
                file.putUserData(MODULE_CACHE_KEY, GhcMod.list(project, workDir));
                file.putUserData(BROWSE_CACHE_KEY, getQualifiedNameCache(file, project, workDir));
            }
        });
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
    public static PsiElement getFirstPrevSiblingWhere(Function<PsiElement, Boolean> f, PsiElement e) {
        return getFirstElementWhere(new Function<PsiElement, PsiElement>() {
            @Override
            public PsiElement fun(PsiElement psiElement) {
                return psiElement.getPrevSibling();
            }
        }, f, e);
    }

    @Nullable
    public static PsiElement getFirstNextSiblingWhere(Function<PsiElement, Boolean> f, PsiElement e) {
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

    public static final Function<String, LookupElement> stringToLookupElement = new Function<String, LookupElement>() {
        @Override
        public LookupElement fun(String s) {
            return LookupElementBuilder.create(s).withIcon(HaskellIcons.FILE);
        }
    };

    public static final Function<Pair<String, String>, LookupElement> pairToLookupElement = new Function<Pair<String, String>, LookupElement>() {
        @Override
        public LookupElement fun(Pair<String, String> pair) {
            return ((LookupElementBuilder)stringToLookupElement.fun(pair.first)).withTypeText(pair.second);
        }
    };
}
