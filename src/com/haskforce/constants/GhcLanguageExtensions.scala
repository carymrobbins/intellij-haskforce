package com.haskforce.constants

import java.util

import com.intellij.codeInsight.lookup.LookupElement

import com.haskforce.codeInsight.LookupElementUtil

/** Language extensions supported by GHC. */
object GhcLanguageExtensions extends Enumeration {

  /** Safer and more efficient alternative to withName() */
  def get(name: String): Option[Value] = Option(mapping.get(name))

  /** Negates the extension value, either adding or removing the "No" prefix, if applicable. */
  def negate(v: Value): Option[Value] = {
    if (v.toString.startsWith("No")) get(v.toString.substring(2)) else get("No" + v.toString)
  }

  /** Retrieve the extensions as LookupElements for completion contributors. */
  lazy val asLookupElements: Set[LookupElement] = values.map(x => LookupElementUtil.fromString(x.toString))

  /** Pre-computed names as an array. */
  lazy val stringArray: Array[String] = values.view.map(_.toString).toArray

  /** Provide faster lookups using a HashMap as opposed to withName(). */
  private lazy val mapping = {
    val m = new util.HashMap[String, Value](values.size)
    values.foreach { v => m.put(v.toString, v) }
    m
  }

  // Extensions list generated with: ghc --show-options | grep '^-X' | cut -dX -f2- | sort
  val AllowAmbiguousTypes
    , AlternativeLayoutRule
    , AlternativeLayoutRuleTransitional
    , ApplicativeDo
    , Arrows
    , AutoDeriveTypeable
    , BangPatterns
    , BinaryLiterals
    , BlockArguments
    , CApiFFI
    , ConstrainedClassMethods
    , ConstraintKinds
    , CPP
    , DataKinds
    , DatatypeContexts
    , DefaultSignatures
    , DeriveAnyClass
    , DeriveDataTypeable
    , DeriveFoldable
    , DeriveFunctor
    , DeriveGeneric
    , DeriveLift
    , DeriveTraversable
    , DerivingStrategies
    , DerivingVia
    , DisambiguateRecordFields
    , DoAndIfThenElse
    , DoRec
    , DuplicateRecordFields
    , EmptyCase
    , EmptyDataDecls
    , EmptyDataDeriving
    , ExistentialQuantification
    , ExplicitForAll
    , ExplicitNamespaces
    , ExtendedDefaultRules
    , FlexibleContexts
    , FlexibleInstances
    , ForeignFunctionInterface
    , FunctionalDependencies
    , GADTs
    , GADTSyntax
    , GeneralisedNewtypeDeriving
    , GeneralizedNewtypeDeriving
    , Generics
    , GHCForeignImportPrim
    , Haskell2010
    , Haskell98
    , HexFloatLiterals
    , ImplicitParams
    , ImplicitPrelude
    , ImpredicativeTypes
    , IncoherentInstances
    , InstanceSigs
    , InterruptibleFFI
    , JavaScriptFFI
    , KindSignatures
    , LambdaCase
    , LiberalTypeSynonyms
    , MagicHash
    , MonadComprehensions
    , MonadFailDesugaring
    , MonoLocalBinds
    , MonomorphismRestriction
    , MonoPatBinds
    , MultiParamTypeClasses
    , MultiWayIf
    , NamedFieldPuns
    , NamedWildCards
    , NegativeLiterals
    , NoAllowAmbiguousTypes
    , NoAlternativeLayoutRule
    , NoAlternativeLayoutRuleTransitional
    , NoApplicativeDo
    , NoArrows
    , NoAutoDeriveTypeable
    , NoBangPatterns
    , NoBinaryLiterals
    , NoBlockArguments
    , NoCApiFFI
    , NoConstrainedClassMethods
    , NoConstraintKinds
    , NoCPP
    , NoDataKinds
    , NoDatatypeContexts
    , NoDefaultSignatures
    , NoDeriveAnyClass
    , NoDeriveDataTypeable
    , NoDeriveFoldable
    , NoDeriveFunctor
    , NoDeriveGeneric
    , NoDeriveLift
    , NoDeriveTraversable
    , NoDerivingStrategies
    , NoDerivingVia
    , NoDisambiguateRecordFields
    , NoDoAndIfThenElse
    , NoDoRec
    , NoDuplicateRecordFields
    , NoEmptyCase
    , NoEmptyDataDecls
    , NoEmptyDataDeriving
    , NoExistentialQuantification
    , NoExplicitForAll
    , NoExplicitNamespaces
    , NoExtendedDefaultRules
    , NoFlexibleContexts
    , NoFlexibleInstances
    , NoForeignFunctionInterface
    , NoFunctionalDependencies
    , NoGADTs
    , NoGADTSyntax
    , NoGeneralisedNewtypeDeriving
    , NoGeneralizedNewtypeDeriving
    , NoGenerics
    , NoGHCForeignImportPrim
    , NoHexFloatLiterals
    , NoImplicitParams
    , NoImplicitPrelude
    , NoImpredicativeTypes
    , NoIncoherentInstances
    , NoInstanceSigs
    , NoInterruptibleFFI
    , NoJavaScriptFFI
    , NoKindSignatures
    , NoLambdaCase
    , NoLiberalTypeSynonyms
    , NoMagicHash
    , NoMonadComprehensions
    , NoMonadFailDesugaring
    , NoMonoLocalBinds
    , NoMonomorphismRestriction
    , NoMonoPatBinds
    , NoMultiParamTypeClasses
    , NoMultiWayIf
    , NoNamedFieldPuns
    , NoNamedWildCards
    , NondecreasingIndentation
    , NoNegativeLiterals
    , NoNondecreasingIndentation
    , NoNPlusKPatterns
    , NoNullaryTypeClasses
    , NoNumDecimals
    , NoNumericUnderscores
    , NoOverlappingInstances
    , NoOverloadedLabels
    , NoOverloadedLists
    , NoOverloadedStrings
    , NoPackageImports
    , NoParallelArrays
    , NoParallelListComp
    , NoPartialTypeSignatures
    , NoPatternGuards
    , NoPatternSignatures
    , NoPatternSynonyms
    , NoPolyKinds
    , NoPolymorphicComponents
    , NoPostfixOperators
    , NoQuantifiedConstraints
    , NoQuasiQuotes
    , NoRank2Types
    , NoRankNTypes
    , NoRebindableSyntax
    , NoRecordPuns
    , NoRecordWildCards
    , NoRecursiveDo
    , NoRelaxedLayout
    , NoRelaxedPolyRec
    , NoRoleAnnotations
    , NoScopedTypeVariables
    , NoStandaloneDeriving
    , NoStarIsType
    , NoStaticPointers
    , NoStrict
    , NoStrictData
    , NoTemplateHaskell
    , NoTemplateHaskellQuotes
    , NoTraditionalRecordSyntax
    , NoTransformListComp
    , NoTupleSections
    , NoTypeApplications
    , NoTypeFamilies
    , NoTypeFamilyDependencies
    , NoTypeInType
    , NoTypeOperators
    , NoTypeSynonymInstances
    , NoUnboxedSums
    , NoUnboxedTuples
    , NoUndecidableInstances
    , NoUndecidableSuperClasses
    , NoUnicodeSyntax
    , NoUnliftedFFITypes
    , NoViewPatterns
    , NPlusKPatterns
    , NullaryTypeClasses
    , NumDecimals
    , NumericUnderscores
    , OverlappingInstances
    , OverloadedLabels
    , OverloadedLists
    , OverloadedStrings
    , PackageImports
    , ParallelArrays
    , ParallelListComp
    , PartialTypeSignatures
    , PatternGuards
    , PatternSignatures
    , PatternSynonyms
    , PolyKinds
    , PolymorphicComponents
    , PostfixOperators
    , QuantifiedConstraints
    , QuasiQuotes
    , Rank2Types
    , RankNTypes
    , RebindableSyntax
    , RecordPuns
    , RecordWildCards
    , RecursiveDo
    , RelaxedLayout
    , RelaxedPolyRec
    , RoleAnnotations
    , Safe
    , ScopedTypeVariables
    , StandaloneDeriving
    , StarIsType
    , StaticPointers
    , Strict
    , StrictData
    , TemplateHaskell
    , TemplateHaskellQuotes
    , TraditionalRecordSyntax
    , TransformListComp
    , Trustworthy
    , TupleSections
    , TypeApplications
    , TypeFamilies
    , TypeFamilyDependencies
    , TypeInType
    , TypeOperators
    , TypeSynonymInstances
    , UnboxedSums
    , UnboxedTuples
    , UndecidableInstances
    , UndecidableSuperClasses
    , UnicodeSyntax
    , UnliftedFFITypes
    , Unsafe
    , ViewPatterns
    = Value
}
