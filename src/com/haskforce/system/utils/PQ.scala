package com.haskforce.system.utils

import scala.collection.JavaConverters._

import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElement
import com.intellij.psi.tree.{IElementType, TokenSet}
import com.intellij.psi.util.PsiTreeUtil

/**
 * IntelliJ Psi Query Util.
 * Safer version of PsiTreeUtil which wraps nullable results in Option.
 */
object PQ {

  def getChildOfType[T <: PsiElement](el: PsiElement, cls: Class[T]): Option[T] = {
    Option(PsiTreeUtil.getChildOfType(el, cls))
  }

  def getChildNodes(el: PsiElement, typ: IElementType, typs: IElementType*): Array[ASTNode] = {
    el.getNode.getChildren(TokenSet.create(typ +: typs: _*))
  }

  def streamChildren[T <: PsiElement](el: PsiElement, cls: Class[T]): Stream[T] = {
    PsiTreeUtil.childIterator(el, cls).asScala.toStream
  }

  /** Analogous to PsiTreeUtil.findFirstParent */
  def collectFirstParent[A]
      (el: PsiElement)(f: PartialFunction[PsiElement, A]): Option[A] = {
    Stream.iterate(el.getParent)(_.getParent).takeWhile(_ != null).collectFirst(f)
  }
}
