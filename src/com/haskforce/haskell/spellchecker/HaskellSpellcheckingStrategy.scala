package com.haskforce.haskell.spellchecker

//TODO refactor
import com.haskforce.tools.cabal.CabalLanguage
import com.haskforce.haskell.HaskellLanguage
import com.intellij.psi.PsiElement
import com.intellij.spellchecker.tokenizer.{SpellcheckingStrategy, Tokenizer}

/**
 * Provide spellchecker support for Haskell/Cabal sources.
 */
//TODO refactor architecture
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
