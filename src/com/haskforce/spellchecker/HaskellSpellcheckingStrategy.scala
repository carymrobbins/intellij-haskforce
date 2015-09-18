package com.haskforce.spellchecker

import com.haskforce.HaskellLanguage
import com.haskforce.cabal.CabalLanguage
import com.intellij.psi.PsiElement
import com.intellij.spellchecker.tokenizer.{Tokenizer, SpellcheckingStrategy}

/**
 * Provide spellchecker support for Haskell/Cabal sources.
 */
class HaskellSpellcheckingStrategy extends SpellcheckingStrategy {
  // Use TEXT_TOKENIZER so prime' names won't be marked as a typo.
  override def getTokenizer(element: PsiElement): Tokenizer[_ <: PsiElement] = {
    SpellcheckingStrategy.TEXT_TOKENIZER
  }

  override def isMyContext(element: PsiElement): Boolean = {
    Seq(
      HaskellLanguage.INSTANCE,
      CabalLanguage.INSTANCE
    ).exists(_.is(element.getLanguage))
  }
}
