package com.haskforce.tools.cabal.spellchecker

import com.haskforce.tools.cabal.CabalLanguage
import com.intellij.psi.PsiElement
import com.intellij.spellchecker.tokenizer.{SpellcheckingStrategy, Tokenizer}

/**
 * Provide spellchecker support for Cabal sources.
 */
class CabalSpellcheckingStrategy extends SpellcheckingStrategy {
  // Use TEXT_TOKENIZER so prime' names won't be marked as a typo.
  override def getTokenizer(element: PsiElement): Tokenizer[_ <: PsiElement] = {
    SpellcheckingStrategy.TEXT_TOKENIZER
  }

  override def isMyContext(element: PsiElement): Boolean = {
    Seq(
      CabalLanguage.INSTANCE
    ).exists(_.is(element.getLanguage))
  }
}
