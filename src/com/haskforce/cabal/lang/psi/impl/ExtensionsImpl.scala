package com.haskforce.cabal.lang.psi.impl

import com.intellij.psi.PsiElement

import com.haskforce.cabal.lang.psi.{CabalTypes, IdentList}
import com.haskforce.utils.PQ

trait ExtensionsImpl extends PsiElement {

  /** Retrieves the extension names as strings. */
  def getValue: Array[String] = PQ.getChildOfType(this, classOf[IdentList]) match {
    case None => Array.empty
    case Some(el) => PQ.getChildNodes(el, CabalTypes.IDENT).map(_.getText)
  }
}
