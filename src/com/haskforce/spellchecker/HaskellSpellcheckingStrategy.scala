package com.haskforce.spellchecker

import com.haskforce.HaskellLanguage
import com.haskforce.cabal.CabalLanguage
import com.intellij.psi.PsiElement
import com.intellij.spellchecker.tokenizer.SpellcheckingStrategy

/**
 * Provide spellchecker support for Haskell/Cabal sources.
 */
class HaskellSpellcheckingStrategy extends SpellcheckingStrategy {

  override def isMyContext(element: PsiElement): Boolean = {
    Seq(
      HaskellLanguage.INSTANCE,
      CabalLanguage.INSTANCE
    ).exists(_.is(element.getLanguage))
  }
}
