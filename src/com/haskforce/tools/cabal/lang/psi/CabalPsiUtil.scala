package com.haskforce.tools.cabal.lang.psi

import com.intellij.psi.PsiElement

import com.haskforce.tools.cabal.lang.psi
import com.haskforce.system.utils.PQ

/**
 * Utilities for traversing the Cabal Psi tree.
 */
object CabalPsiUtil {

  def getFieldContext(el: PsiElement): Option[psi.CabalFieldElement] = {
    PQ.collectFirstParent(el) { case el: psi.CabalFieldElement => el }
  }
}
