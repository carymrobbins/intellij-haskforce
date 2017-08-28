package prelude.types

import prelude._

import com.haskforce.utils.{CovariantNewType, IJReadAction}
import com.intellij.psi.{PsiElement, PsiFile}

import scala.reflect.ClassTag

/**
 * Safer newtype wrapper around PsiFile.
 * See the `types` package object for concrete types built from this wrapper.
 */
object SPsiFile extends CovariantNewType.Default[PsiFile] {
  implicit final class Ops[A <: PsiFile](val self: Type[A]) extends AnyVal {

    def toPsiFile: A = unwrap(self)

    def asElement: SPsiElement[A] = SPsiElement(toPsiFile)

    def getVirtualFile: Option[SVirtualFile] = Option(SVirtualFile(toPsiFile.getVirtualFile))

    def getChildren: IJReadAction[Array[SPsiElement.Top]] = SPsiElementUtil.getChildren(toPsiFile)

    def getFirstChild: IJReadAction[Option[SPsiElement.Top]] = SPsiElementUtil.getFirstChild(toPsiFile)

    def getChildOfType[T <: PsiElement : ClassTag]: IJReadAction[Option[SPsiElement[T]]]
      = SPsiElementUtil.getChildOfType[T](toPsiFile)

    def getChildrenOfType[T <: PsiElement : ClassTag]: IJReadAction[Vector[SPsiElement[T]]]
      = SPsiElementUtil.getChildrenOfType[T](toPsiFile)

    def getText: IJReadAction[String] = SPsiElementUtil.getText(toPsiFile)
  }
}
