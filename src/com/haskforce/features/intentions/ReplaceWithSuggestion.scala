package com.haskforce.features.intentions

import com.haskforce.haskell.lang.parser.HaskellTokenTypes2020
import com.intellij.codeInsight.intention.impl.BaseIntentionAction
import com.intellij.lang.annotation.Annotation
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import com.intellij.psi.impl.source.tree.LeafPsiElement
import com.intellij.psi.tree.IElementType

class ReplaceWithSuggestion(
  suggestion: String,
  el: LeafPsiElement
) extends BaseIntentionAction {

  override def getFamilyName: String =
    "Replace typo with suggestion"

  override def getText: String =
    s"Replace with: $suggestion"

  override def isAvailable(project: Project, editor: Editor, file: PsiFile): Boolean =
    // TODO: Add setting
    true

  override def invoke(project: Project, editor: Editor, file: PsiFile): Unit = {
    el.replaceWithText(suggestion)
    ()
  }
}

object ReplaceWithSuggestion {

  def registerFixes(suggestion: String, annotation: Annotation, psiFile: PsiFile): Unit = {
    val offset = annotation.getStartOffset
    getTargetElement(psiFile, offset).foreach { el =>
      annotation.registerFix(new ReplaceWithSuggestion(suggestion, el))
    }
  }

  private def getTargetElement(psiFile: PsiFile, offset: Int): Option[LeafPsiElement] = {
    psiFile.findElementAt(offset) match {
      case null => None
      case el: LeafPsiElement =>
        val typ = el.getElementType
        if (supportedElementTypes.contains(typ)) Some(el) else None
    }
  }

  private val supportedElementTypes = Set[IElementType](
    HaskellTokenTypes2020.CONIDREGEXP,
    HaskellTokenTypes2020.VARIDREGEXP
  )
}
