package com.haskforce.tools.cabal.completion

import com.google.common.collect.Iterables
import com.intellij.codeInsight.completion._
import com.intellij.codeInsight.lookup.LookupElement
import com.intellij.patterns.PlatformPatterns
import com.intellij.psi.PsiElement
import com.intellij.util.ProcessingContext

import com.haskforce.tools.cabal.CabalLanguage
import com.haskforce.tools.cabal.lang.psi
import com.haskforce.tools.cabal.lang.psi.CabalPsiUtil
import com.haskforce.haskell.constants.GhcLanguageExtensions
import com.haskforce.system.utils.SAMUtils

final class CabalCompletionContributor extends CompletionContributor {

  // Extends this class with completion for Cabal files.
  extend(
    CompletionType.BASIC,
    PlatformPatterns.psiElement().withLanguage(CabalLanguage.INSTANCE),
    newProvider()
  )

  private def newProvider() = new CompletionProvider[CompletionParameters]() {
    override def addCompletions
        (parameters: CompletionParameters,
         context: ProcessingContext,
         result: CompletionResultSet)
        : Unit
        = new CompletionWrapper(parameters, context, result).run()
  }

  private class CompletionWrapper(
    parameters: CompletionParameters,
    context: ProcessingContext,
    result: CompletionResultSet
  ) {

    def run(): Unit = {
      if (completeExtensions()) return
    }

    lazy val position: PsiElement = parameters.getPosition

    def completeExtensions(): Boolean = {
      CabalPsiUtil.getFieldContext(position).collect {
        case el: psi.impl.ExtensionsImpl => result.addAllElements(filterExtensions(el))
      }.isDefined
    }

    /** Skip already provided extensions or their negation. */
    def filterExtensions(el: psi.impl.ExtensionsImpl): java.lang.Iterable[LookupElement] = {
      val currentExts = el.getValue.toSet.flatMap(GhcLanguageExtensions.get)
      val negExts = currentExts.flatMap(GhcLanguageExtensions.negate)
      val skipExts = (currentExts ++ negExts).map(_.toString)
      Iterables.filter(
        GhcLanguageExtensions.asLookupElements,
        SAMUtils.guava.predicate[LookupElement](x => !skipExts.contains(x.getLookupString))
      )
    }
  }
}
