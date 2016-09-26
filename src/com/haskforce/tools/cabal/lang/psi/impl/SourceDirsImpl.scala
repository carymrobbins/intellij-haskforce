package com.haskforce.tools.cabal.lang.psi.impl

import com.intellij.psi.PsiElement

import com.haskforce.tools.cabal.lang.psi.CabalTypes
import com.haskforce.system.utils.PQ

trait SourceDirsImpl extends PsiElement {

  /** Retrieves the source dir paths as strings. */
  def getValue: Array[String] = {
    PQ.getChildNodes(this, CabalTypes.SOURCE_DIR).map(_.getText)
  }
}
