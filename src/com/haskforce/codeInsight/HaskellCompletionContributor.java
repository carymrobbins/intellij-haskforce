package com.haskforce.codeInsight;

import com.haskforce.HaskellLanguage;
import com.haskforce.language.HaskellNamesValidator;
import com.haskforce.psi.HaskellTypes;
import com.haskforce.utils.LogicUtil;
import com.intellij.codeInsight.completion.CompletionContributor;
import com.intellij.codeInsight.completion.CompletionParameters;
import com.intellij.codeInsight.completion.CompletionProvider;
import com.intellij.codeInsight.completion.CompletionResultSet;
import com.intellij.codeInsight.completion.CompletionType;
import com.intellij.codeInsight.lookup.LookupElementBuilder;
import com.intellij.openapi.editor.Editor;
import com.intellij.patterns.PlatformPatterns;
import com.intellij.util.Function;
import com.intellij.util.ProcessingContext;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.Arrays;
import java.util.List;

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
                        for (String word : HaskellNamesValidator.HASKELL_KEYWORDS) {
                            result.addElement(LookupElementBuilder.create(word));
                        }
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
                        result.addElement(LookupElementBuilder.create("LANGUAGE"));
                        result.addAllElements(LANGUAGE_EXTENSIONS);
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

    public static final List<LookupElementBuilder> LANGUAGE_EXTENSIONS = LogicUtil.map(new Function<String, LookupElementBuilder>() {
        @Override
        public LookupElementBuilder fun(String s) {
            return LookupElementBuilder.create(s);
        }
    }, Arrays.asList(
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
