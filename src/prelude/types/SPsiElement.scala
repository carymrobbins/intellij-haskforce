package prelude.types

import prelude._

import com.haskforce.utils.{CovariantNewType, IJReadAction}
import com.intellij.lang.ASTNode
import com.intellij.psi.PsiElement
import com.intellij.psi.tree.{IElementType, TokenSet}

import scala.reflect.ClassTag

object SPsiElement extends CovariantNewType.Default[PsiElement] {

  implicit final class Ops[A <: PsiElement](val self: Type[A]) extends AnyVal {

    def toPsiElement: A = unwrap(self)

    def getChildren: IJReadAction[Array[SPsiElement.Top]] = SPsiElementUtil.getChildren(toPsiElement)

    def getFirstChild: IJReadAction[Option[SPsiElement.Top]] = SPsiElementUtil.getFirstChild(toPsiElement)

    def getChildOfType[T <: PsiElement : ClassTag]: IJReadAction[Option[SPsiElement[T]]]
      = SPsiElementUtil.getChildOfType[T](toPsiElement)

    def getChildrenOfType[T <: PsiElement : ClassTag]: IJReadAction[Vector[SPsiElement[T]]]
    = SPsiElementUtil.getChildrenOfType[T](toPsiElement)

    def getText: IJReadAction[String] = SPsiElementUtil.getText(toPsiElement)

    def getChildNodes(typ: IElementType, typs: IElementType*): IJReadAction[Array[ASTNode]]
      = IJReadAction(toPsiElement.getNode.getChildren(TokenSet.create(typ +: typs: _*)))
  }
}

/** Utilities for implementing SPsiElement and SPsiFile. */
object SPsiElementUtil {

  def getChildren(e: PsiElement): IJReadAction[Array[SPsiElement.Top]]
    = IJReadAction(SPsiElement.applyM[Array, PsiElement](e.getChildren))

  def getFirstChild(e: PsiElement): IJReadAction[Option[SPsiElement.Top]]
    = IJReadAction(Option(SPsiElement(e.getFirstChild)))

  def getChildOfType[T <: PsiElement](e: PsiElement)(
    implicit ct: ClassTag[T]
  ): IJReadAction[Option[SPsiElement[T]]] = getChildren(e).map(children =>
    children.iterator.map(ct.unapply).collectFirst { case Some(t) => SPsiElement(t) }
  )

  def getChildrenOfType[T <: PsiElement](e: PsiElement)(
    implicit ct: ClassTag[T]
  ): IJReadAction[Vector[SPsiElement[T]]] = getChildren(e).map(children =>
    SPsiElement.applyM(children.iterator.flatMap(ct.unapply(_)).toVector)
  )

  def getText(e: PsiElement): IJReadAction[String] = IJReadAction(e.getText)
}
