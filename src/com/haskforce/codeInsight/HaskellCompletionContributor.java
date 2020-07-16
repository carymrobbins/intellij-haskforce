package com.haskforce.codeInsight;

import com.google.common.base.Predicate;
import com.google.common.collect.Iterables;
import com.haskforce.HaskellLanguage;
import com.haskforce.codeInsight.HaskellCompletionCacheLoader.Cache;
import com.haskforce.codeInsight.HaskellCompletionCacheLoader.LookupElementWrapper;
import com.haskforce.codeInsight.visibleModules.VisibleModulesProvider;
import com.haskforce.codeInsight.visibleModules.VisibleModulesProviderFactory;
import com.haskforce.psi.*;
import com.haskforce.utils.HaskellUtil;
import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.diagnostic.Logger;
import com.intellij.openapi.editor.Editor;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.psi.*;
import com.intellij.psi.util.PsiTreeUtil;
import com.intellij.util.ArrayUtil;
import com.intellij.util.Function;
import com.intellij.util.ProcessingContext;
import com.intellij.util.containers.ContainerUtil;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import scala.Option;

import java.util.*;

/**
 * Fills the list of completions available on ctrl-space.
 */
public class HaskellCompletionContributor extends CompletionContributor {
    @SuppressWarnings("UnusedDeclaration")
    private static final Logger LOG = Logger.getInstance(HaskellCompletionContributor.class);

    private static String[] PRAGMA_TYPES = new String[]{
            "LANGUAGE ", "OPTIONS_GHC ", "WARNING ", "DEPRECATED ", "INLINE ", "NOINLINE ", "INLINABLE ", "CONLIKE ",
            "RULES ", "ANN ", "LINE ", "SPECIALIZE ", "UNPACK ", "SOURCE "};

    public static String[] getPragmaTypes() {
        return PRAGMA_TYPES.clone();
    }

    public HaskellCompletionContributor() {
        extend(CompletionType.BASIC,
                PlatformPatterns.psiElement().withLanguage(HaskellLanguage.INSTANCE),
                new HaskellCacheReloadCompletionProvider()
        );
        extend(CompletionType.BASIC,
                PlatformPatterns.psiElement().withLanguage(HaskellLanguage.INSTANCE),
                new CompletionProvider<CompletionParameters>() {
                    public void addCompletions(@NotNull CompletionParameters parameters,
                                               ProcessingContext context,
                                               @NotNull CompletionResultSet result) {
                        PsiElement position = parameters.getPosition();
                        PsiFile file = parameters.getOriginalFile();
                        List<HaskellPsiUtil.Import> imports = HaskellPsiUtil.parseImports(file);
                        Cache cache = getCache(file);

                        // TODO: A bit of a hack; should fix this up into Cache above somehow.
                        Option<VisibleModulesProvider> visibleModulesProvider = VisibleModulesProviderFactory.get(file);

                        // Completion methods should return either void or boolean.  If boolean, then it should indicate
                        // whether or not we were in the appropriate context.  This is useful to determine if following
                        // completions should be added.
                        completeKeywordImport(position, result);
                        completeKeywordQualified(position, result);
                        if (completePragma(position, cache, result)) return;
                        if (completeModuleImport(position, visibleModulesProvider, result)) return;
                        if (completeQualifiedNames(position, imports, cache, result)) return;
                        if (completeNameImport(position, cache, result)) return;
                        completeFunctionDeclName(position, result);
                        completeExpressionKeywords(position, result);
                        completeLocalNames(position, imports, cache, result);
                        completeFunctionLocalNames(position,result);
                    }
                }
        );
    }

    private static Cache getCache(PsiFile file) {
        return HaskellCompletionCacheLoader.getService(file.getProject()).cache();
    }

    public static void completeKeywordImport(@NotNull final PsiElement position, @NotNull final CompletionResultSet result) {
        if (PsiTreeUtil.getParentOfType(position, HaskellImpdecl.class) != null) return;
        HaskellBody body = PsiTreeUtil.getParentOfType(position, HaskellBody.class);
        PsiElement root = body == null ? position.getContainingFile() : body;
        PsiElement topLevel = PsiTreeUtil.findPrevParent(root, position);
        // If we have spaces, then we are into an expression, definition, etc. and shouldn't provide completion.
        if (topLevel.getText().contains(" ")) return;
        for (PsiElement child : root.getChildren()) {
            if (PsiTreeUtil.instanceOf(child,
                    HaskellPpragma.class, HaskellImpdecl.class, PsiWhiteSpace.class, PsiComment.class)) continue;
            // If something else other than the allowed elements appear before our element, don't provide completion.
            if (!child.equals(topLevel)) return;
        }
        result.addElement(LookupElementUtil.fromString("import "));
    }

    public static void completeKeywordQualified(@NotNull final PsiElement position, @NotNull final CompletionResultSet result) {
        final PsiElement prevLeaf = PsiTreeUtil.prevVisibleLeaf(position);
        if (prevLeaf != null && prevLeaf.getText().equals("import")) {
            result.addElement(LookupElementUtil.fromString("qualified "));
        }
    }

    private static String[] EXPRESSION_KEYWORDS = {"do", "if", "then", "else"};
    public static void completeExpressionKeywords(@NotNull final PsiElement position, @NotNull final CompletionResultSet result) {
        if (PsiTreeUtil.getParentOfType(position, HaskellExp.class) == null) return;
        for (String keyword : EXPRESSION_KEYWORDS) {
            result.addElement(LookupElementUtil.fromString(keyword));
        }
    }

    public static boolean completePragma(@NotNull final PsiElement position,
                                         @NotNull final Cache cache,
                                         @NotNull final CompletionResultSet result) {
        final PsiElement prevSibling = getPrevSiblingWhere(new Function<PsiElement, Boolean>() {
            @Override
            public Boolean fun(PsiElement psiElement) {
                return !(psiElement instanceof PsiWhiteSpace);
            }
        }, position);

        // Pragma types.
        if (prevSibling != null && "{-#".equals(prevSibling.getText())) {
            addAllElements(result, LookupElementUtil.fromStrings(PRAGMA_TYPES));
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
            return false;
        }

        final String pragmaType = pragmaTypeElement.getText();

        if ("LANGUAGE".equals(pragmaType)) {
            addAllElements(result, cache.languageExtensions());
        } else if ("OPTIONS_GHC".equals(pragmaType)) {
            // TODO: Workaround since completion autocompletes after the "-", so without this
            // we may end up completing -foo with --foo (inserting a "-").
            final Set<String> flags = cache.ghcFlags();
            if (flags != null) {
                if (position.getText().startsWith("-")) {
                    addAllElements(result, ContainerUtil.map(flags, new Function<String, LookupElement>() {
                        @Override
                        public LookupElement fun(String s) {
                            return LookupElementUtil.fromString(s.startsWith("-") ? s.substring(1) : s);
                        }
                    }));
                } else {
                    addAllElements(result, LookupElementUtil.fromStrings(flags));
                }
            }
        }
        return true;
    }

    public static boolean completeModuleImport(@NotNull final PsiElement position,
                                               @NotNull final Option<VisibleModulesProvider> visibleModulesProvider,
                                               @NotNull final CompletionResultSet result) {
        // TODO: Refactor this implementation.
        PsiElement el = position.getParent();
        if (!(el instanceof HaskellConid)) {
            return false;
        }
        el = el.getParent();
        if (!(el instanceof HaskellQconid)) {
            return false;
        }
        el = el.getParent();
        if (!(el instanceof HaskellImpdecl)) {
            return false;
        }
        // Regardless of whether we actually have cache data to work with, we still want to return true
        // after this point since we've already identified that we are in the appropriate context.
        final String[] list = visibleModulesProvider.fold(() -> null, VisibleModulesProvider::getVisibleModules);
        if (list != null && list.length != 0) {
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
            addAllElements(result, LookupElementUtil.fromStrings(newLines));
        }
        return true;
    }

    public static boolean completeNameImport(@NotNull final PsiElement position,
                                             @NotNull final Cache cache,
                                             @NotNull final CompletionResultSet result) {
        // Ensure we are in an import name element.
        if (PsiTreeUtil.getParentOfType(position, HaskellImportt.class) == null) return false;
        HaskellImpdecl impdecl = PsiTreeUtil.getParentOfType(position, HaskellImpdecl.class);
        if (impdecl == null) return true;
        HaskellQconid qconid = PsiTreeUtil.findChildOfType(impdecl, HaskellQconid.class);
        if (qconid == null) return true;
        final String module = qconid.getText();
        final Set<LookupElementWrapper> cachedNames = cache.moduleSymbols().get(module);
        addAllElements(result, cachedNames);
        return true;
    }

    /** Used for completing a function name after its type signature or definition. */
    public static void completeFunctionDeclName(@NotNull final PsiElement position,
                                                @NotNull final CompletionResultSet result) {
        // Find the root node at the current offset so we can find the previous element.
        PsiElement e = position;
        while (true) {
            if (e.getParent() == null) return;
            if (e.getParent().getTextOffset() != position.getTextOffset()) break;
            e = e.getParent();
        }
        // Now find the previous sibling until we don't have a whitespace.
        e = e.getPrevSibling();
        while (e != null) {
            if (!(e instanceof PsiWhiteSpace)) break;
            e = e.getPrevSibling();
        }
        if (e == null) return;
        String name = null;
        // Now check if the previous node is a function type signature or definition,
        // obtaining the function name if possible.
        if (e instanceof HaskellGendecl) {
            HaskellGendecl g = (HaskellGendecl)e;
            HaskellVars v = g.getVars();
            if (v == null) return;
            List<HaskellVarid> vs = v.getVaridList();
            if (vs.size() == 0) return;
            name = vs.get(0).getName();
        } else if (e instanceof HaskellFunorpatdecl) {
            HaskellFunorpatdecl f = (HaskellFunorpatdecl)e;
            List<HaskellPat> ps = f.getPatList();
            if (ps.size() == 0) return;
            List<HaskellVarid> vs = ps.get(0).getVaridList();
            if (vs.size() == 0) return;
            name = vs.get(0).getName();
        }
        if (name == null) return;
        addAllElements(result, Collections.singletonList(LookupElementBuilder.create(name)));
    }

    public static void completeFunctionLocalNames(@NotNull final PsiElement position,
                                                  @NotNull final CompletionResultSet result){
        List<PsiElement> allDefinitionsInScope = HaskellUtil.getAllDefinitionsInScope(position);
        for (PsiElement psiElement : allDefinitionsInScope) {
            result.addElement(LookupElementBuilder.create((PsiNamedElement)psiElement));
        }
        List<PsiElement> allDefinitionsInWhereClausesInScope = HaskellUtil.getAllDefinitionsInWhereClausesInScope(position);
        for (PsiElement psiElement : allDefinitionsInWhereClausesInScope) {
            result.addElement(LookupElementBuilder.create((PsiNamedElement)psiElement));
        }
    }

    public static boolean completeQualifiedNames(@NotNull final PsiElement position,
                                                 @NotNull final List<HaskellPsiUtil.Import> imports,
                                                 @NotNull final Cache cacheHolder,
                                                 @NotNull final CompletionResultSet result) {
        PsiElement el = position.getParent();
        if (el == null) {
            return false;
        }
        el = el.getParent();
        if (!(el instanceof HaskellQconid || el instanceof HaskellQvarid)) {
            return false;
        }
        final String qName = el.getText();
        final int lastDotPos = qName.lastIndexOf('.');
        if (lastDotPos == -1) {
            return false;
        }
        final String alias = qName.substring(0, lastDotPos);
        // Pull user-qualified names from cache.
        final Map<String, Set<LookupElementWrapper>> browseCache = cacheHolder.moduleSymbols();
        if (browseCache != null) {
            final Iterable<HaskellPsiUtil.Import> filteredImports = Iterables.filter(imports, new Predicate<HaskellPsiUtil.Import>() {
                @Override
                public boolean apply(HaskellPsiUtil.Import anImport) {
                    return anImport != null && alias.equals(anImport.alias);
                }
            });
            final HaskellPsiUtil.Import anImport = Iterables.getFirst(filteredImports, null);
            if (anImport != null) {
                addAllElements(result, browseCache.get(anImport.module));
            }
        }
        return true;
    }

    public static boolean completeLocalNames(@NotNull final PsiElement position,
                                             @NotNull final List<HaskellPsiUtil.Import> imports,
                                             @NotNull final Cache holder,
                                             @NotNull final CompletionResultSet result) {
        if (PsiTreeUtil.getParentOfType(position, HaskellExp.class) == null) {
            return false;
        }
        final Map<String, Set<LookupElementWrapper>> cachedNames = holder.moduleSymbols();
        if (cachedNames == null) {
            return false;
        }
        for (HaskellPsiUtil.Import anImport : imports) {
            Set<LookupElementWrapper> names = cachedNames.get(anImport.module);
            if (names == null) continue;
            String[] importedNames = anImport.getImportedNames();
            String[] hidingNames = anImport.getHidingNames();
            for (LookupElementWrapper cachedName : names) {
                String lookupString = cachedName.get().getLookupString();
                boolean noExplicitNames = importedNames == null;
                boolean isImportedName = importedNames != null && ArrayUtil.contains(lookupString, importedNames);
                boolean isHidingName = hidingNames != null && ArrayUtil.contains(lookupString, hidingNames);
                if ((noExplicitNames || isImportedName) && !isHidingName) {
                    result.addElement(cachedName.get());
                }
            }
        }
        return true;
    }

    /**
     * Helper to prevent having to do a null check before adding elements to the completion result.
     */
    public static void addAllElements(CompletionResultSet result, List<LookupElement> elements) {
        if (elements != null) {
            result.addAllElements(elements);
        }
    }

    public static void addAllElements(CompletionResultSet result, Set<LookupElementWrapper> elements) {
        if (elements == null) return;
        for (LookupElementWrapper el : elements) {
            result.addElement(el.get());
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
}
