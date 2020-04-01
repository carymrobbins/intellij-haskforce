package com.haskforce.utils

import com.intellij.lang.ASTNode
import com.intellij.psi.tree.{IElementType, TokenSet}
import com.intellij.psi.util.PsiTreeUtil
import com.intellij.psi.{PsiElement, SyntaxTraverser}

import scala.collection.JavaConverters._
import scala.reflect.ClassTag

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
    SyntaxTraverser.psiTraverser().children(el).filter(cls).iterator().asScala.toStream
  }

  /** Analogous to PsiTreeUtil.findFirstParent */
  def findFirstParent[T <: PsiElement](el: PsiElement)(implicit ct: ClassTag[T]): Option[T] = {
    Option(PsiTreeUtil.findFirstParent(
      el,
      (t: PsiElement) => ct.unapply(t).isDefined)
    ).asInstanceOf[Option[T]]
  }

  /** Analogous to PsiTreeUtil.findFirstParent but via a partial function. */
  def collectFirstParent[A](el: PsiElement)(f: PartialFunction[PsiElement, A]): Option[A] = {
    Option(PsiTreeUtil.findFirstParent(el, (t: PsiElement) => f.isDefinedAt(t))).map(f)
  }
}
