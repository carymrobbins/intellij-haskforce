package com.haskforce.cabal.lang.psi.impl

import com.intellij.psi.PsiElement

import com.haskforce.cabal.lang.psi._
import com.haskforce.utils.PQ

trait BuildDependsImpl extends PsiElement {

  /** Retrieves the package names as strings. */
  def getPackageNames: Array[String] = PQ.getChildOfType(this, classOf[Dependencies]) match {
    case None => Array.empty
    case Some(el) =>
      val res =
        PQ.streamChildren(el, classOf[Dependency]).flatMap(c =>
          PQ.getChildNodes(c, CabalTypes.DEPENDENCY_NAME).headOption.map(_.getText)
        ).toArray
      res
  }
}
