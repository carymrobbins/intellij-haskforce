package com.haskforce.codeInsight;

import com.haskforce.HaskellLanguage;
import com.haskforce.psi.HaskellConid;
import com.haskforce.psi.HaskellImpdecl;
import com.haskforce.psi.HaskellQconid;
import com.haskforce.psi.HaskellTypes;
import com.haskforce.utils.ExecUtil;
import com.haskforce.utils.LogicUtil;
import com.intellij.codeInsight.completion.*;
import com.intellij.codeInsight.lookup.LookupElement;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.editor.Editor;
import com.intellij.openapi.util.text.StringUtil;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.psi.PsiElement;
import com.intellij.util.Function;
import com.intellij.util.ProcessingContext;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * Fills the list of completions available on ctrl-space.
 */
public class HaskellCompletionContributor extends CompletionContributor {
    public HaskellCompletionContributor() {
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
                        result.addAllElements(LogicUtil.map(stringToLookupElement, new String[]{
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
                        // TODO: Add other pragma types and ghc flags (preferably with `ghc-mod flag`).
                        result.addElement(LookupElementBuilder.create("LANGUAGE"));
                        result.addAllElements(LANGUAGE_EXTENSIONS);
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
                        final String list = position.getContainingFile().getOriginalFile().getUserData(ExecUtil.MODULE_CACHE_KEY);
                        if (list == null) {
                            return;
                        }
                        String partialModule = "";
                        el = position.getParent();
                        while (el != null) {
                            el = el.getPrevSibling();
                            if (el != null) {
                                partialModule = el.getText() + partialModule;
                            }
                        }
                        List<String> lines = Arrays.asList(StringUtil.splitByLines(list));
                        Set<String> newLines = new HashSet<String>(0);
                        for (String line : lines) {
                            if (line.startsWith(partialModule)) {
                                String newLine = line.replace(partialModule, "");
                                final int firstDotPos = newLine.indexOf('.');
                                if (firstDotPos != -1) {
                                    newLine = newLine.substring(0, firstDotPos);
                                }
                                newLines.add(newLine);
                            }
                        }
                        result.addAllElements(LogicUtil.map(stringToLookupElement, newLines));
                    }
                });
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
            return LookupElementBuilder.create(s);
        }
    };

    // TODO: Tie into ghc-mod for the list of language extensions.
    public static final List<LookupElement> LANGUAGE_EXTENSIONS = LogicUtil.map(stringToLookupElement, Arrays.asList(
            "Haskell98",
            "Haskell2010",
            "Unsafe",
            "Trustworthy",
            "Safe",
            "CPP",
            "NoCPP",
            "PostfixOperators",
            "NoPostfixOperators",
            "TupleSections",
            "NoTupleSections",
            "PatternGuards",
            "NoPatternGuards",
            "UnicodeSyntax",
            "NoUnicodeSyntax",
            "MagicHash",
            "NoMagicHash",
            "ExistentialQuantification",
            "NoExistentialQuantification",
            "KindSignatures",
            "NoKindSignatures",
            "RoleAnnotations",
            "NoRoleAnnotations",
            "EmptyDataDecls",
            "NoEmptyDataDecls",
            "ParallelListComp",
            "NoParallelListComp",
            "TransformListComp",
            "NoTransformListComp",
            "MonadComprehensions",
            "NoMonadComprehensions",
            "ForeignFunctionInterface",
            "NoForeignFunctionInterface",
            "UnliftedFFITypes",
            "NoUnliftedFFITypes",
            "InterruptibleFFI",
            "NoInterruptibleFFI",
            "CApiFFI",
            "NoCApiFFI",
            "GHCForeignImportPrim",
            "NoGHCForeignImportPrim",
            "JavaScriptFFI",
            "NoJavaScriptFFI",
            "LiberalTypeSynonyms",
            "NoLiberalTypeSynonyms",
            "PolymorphicComponents",
            "NoPolymorphicComponents",
            "Rank2Types",
            "NoRank2Types",
            "RankNTypes",
            "NoRankNTypes",
            "ImpredicativeTypes",
            "NoImpredicativeTypes",
            "TypeOperators",
            "NoTypeOperators",
            "ExplicitNamespaces",
            "NoExplicitNamespaces",
            "RecursiveDo",
            "NoRecursiveDo",
            "DoRec",
            "NoDoRec",
            "Arrows",
            "NoArrows",
            "ParallelArrays",
            "NoParallelArrays",
            "TemplateHaskell",
            "NoTemplateHaskell",
            "QuasiQuotes",
            "NoQuasiQuotes",
            "ImplicitPrelude",
            "NoImplicitPrelude",
            "RecordWildCards",
            "NoRecordWildCards",
            "NamedFieldPuns",
            "NoNamedFieldPuns",
            "RecordPuns",
            "NoRecordPuns",
            "DisambiguateRecordFields",
            "NoDisambiguateRecordFields",
            "OverloadedStrings",
            "NoOverloadedStrings",
            "NumDecimals",
            "NoNumDecimals",
            "OverloadedLists",
            "NoOverloadedLists",
            "GADTs",
            "NoGADTs",
            "GADTSyntax",
            "NoGADTSyntax",
            "ViewPatterns",
            "NoViewPatterns",
            "TypeFamilies",
            "NoTypeFamilies",
            "BangPatterns",
            "NoBangPatterns",
            "MonomorphismRestriction",
            "NoMonomorphismRestriction",
            "NPlusKPatterns",
            "NoNPlusKPatterns",
            "DoAndIfThenElse",
            "NoDoAndIfThenElse",
            "RebindableSyntax",
            "NoRebindableSyntax",
            "ConstraintKinds",
            "NoConstraintKinds",
            "PolyKinds",
            "NoPolyKinds",
            "DataKinds",
            "NoDataKinds",
            "InstanceSigs",
            "NoInstanceSigs",
            "MonoPatBinds",
            "NoMonoPatBinds",
            "ExplicitForAll",
            "NoExplicitForAll",
            "AlternativeLayoutRule",
            "NoAlternativeLayoutRule",
            "AlternativeLayoutRuleTransitional",
            "NoAlternativeLayoutRuleTransitional",
            "DatatypeContexts",
            "NoDatatypeContexts",
            "NondecreasingIndentation",
            "NoNondecreasingIndentation",
            "RelaxedLayout",
            "NoRelaxedLayout",
            "TraditionalRecordSyntax",
            "NoTraditionalRecordSyntax",
            "LambdaCase",
            "NoLambdaCase",
            "MultiWayIf",
            "NoMultiWayIf",
            "MonoLocalBinds",
            "NoMonoLocalBinds",
            "RelaxedPolyRec",
            "NoRelaxedPolyRec",
            "ExtendedDefaultRules",
            "NoExtendedDefaultRules",
            "ImplicitParams",
            "NoImplicitParams",
            "ScopedTypeVariables",
            "NoScopedTypeVariables",
            "AllowAmbiguousTypes",
            "NoAllowAmbiguousTypes",
            "PatternSignatures",
            "NoPatternSignatures",
            "UnboxedTuples",
            "NoUnboxedTuples",
            "StandaloneDeriving",
            "NoStandaloneDeriving",
            "DeriveDataTypeable",
            "NoDeriveDataTypeable",
            "AutoDeriveTypeable",
            "NoAutoDeriveTypeable",
            "DeriveFunctor",
            "NoDeriveFunctor",
            "DeriveTraversable",
            "NoDeriveTraversable",
            "DeriveFoldable",
            "NoDeriveFoldable",
            "DeriveGeneric",
            "NoDeriveGeneric",
            "DefaultSignatures",
            "NoDefaultSignatures",
            "TypeSynonymInstances",
            "NoTypeSynonymInstances",
            "FlexibleContexts",
            "NoFlexibleContexts",
            "FlexibleInstances",
            "NoFlexibleInstances",
            "ConstrainedClassMethods",
            "NoConstrainedClassMethods",
            "MultiParamTypeClasses",
            "NoMultiParamTypeClasses",
            "NullaryTypeClasses",
            "NoNullaryTypeClasses",
            "FunctionalDependencies",
            "NoFunctionalDependencies",
            "GeneralizedNewtypeDeriving",
            "NoGeneralizedNewtypeDeriving",
            "OverlappingInstances",
            "NoOverlappingInstances",
            "UndecidableInstances",
            "NoUndecidableInstances",
            "IncoherentInstances",
            "NoIncoherentInstances",
            "PackageImports",
            "NoPackageImports",
            "NegativeLiterals",
            "NoNegativeLiterals",
            "EmptyCase",
            "NoEmptyCase",
            "PatternSynonyms",
            "NoPatternSynonyms"
    ));
}
