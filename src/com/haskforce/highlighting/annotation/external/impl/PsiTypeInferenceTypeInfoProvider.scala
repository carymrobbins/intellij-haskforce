package com.haskforce.highlighting.annotation.external
package impl

import com.haskforce.psi.impl.HaskellPsiImplUtil
import com.haskforce.psi.{HaskellConid, HaskellGendecl, HaskellVarid}
import com.intellij.psi.PsiElement

/**
  * Very simple type inferencer that attempts to determine the type of
  * an expression from the AST alone to avoid calling out to external tools
  * when unnecessary.
  */
final class PsiTypeInferenceTypeInfoProvider(
  input: TypeInfoProviderFactory.Input
) extends TypeInfoProvider {

  override def getTypeInfo: TypeInfoProvider.Result = {
    val el = input.psiFile.findElementAt(input.offsetStart)
    if (el == null) return TypeInfoProvider.Result.failure("Psi element not found")
    val elStartPos = input.editor.offsetToLogicalPosition(el.getTextRange.getStartOffset)
    val elStopPos = input.editor.offsetToLogicalPosition(el.getTextRange.getEndOffset)
    // Check if our element covers the entire selection.
    if (input.selectionStart.compareTo(elStartPos) >= 0 &&
        input.selectionStop.compareTo(elStopPos)   <= 0) {
      return inferTypeOf(el)
    }
    // Otherwise, we've selected multiple elements.
    TypeInfoProvider.Result.failure("Cannot infer type of multiple psi elements")
  }

  // Infers the type of `el` by resolving its reference and looking for an
  // explicit type annotation.
  private def inferTypeOf(el: PsiElement): TypeInfoProvider.Result = {
    TypeInfoProvider.Result.fromOption(
      resolveReference(el).flatMap(optGetParent).flatMap(optGetParent).flatMap {
        case g: HaskellGendecl => Option(g.getCtype).map(_.getText)
        case _ => None
      },
      "Failed to infer type"
    )
  }

  private def optGetParent(el: PsiElement): Option[PsiElement] =
    Option(el.getParent)

  private def resolveReference(el: PsiElement): Option[PsiElement] =
    el.getParent match {
      case x: HaskellVarid => Option(HaskellPsiImplUtil.getReference(x).resolve())
      case x: HaskellConid => Option(HaskellPsiImplUtil.getReference(x).resolve())
      case _ => None
    }
}

object PsiTypeInferenceTypeInfoProviderFactory extends TypeInfoProviderFactory {
  override def get(input: TypeInfoProviderFactory.Input): Option[TypeInfoProvider] =
    Some(new PsiTypeInferenceTypeInfoProvider(input))
}
