package com.haskforce.features.intentions

import java.util.regex.{Matcher, Pattern}

import com.haskforce.psi.impl.HaskellElementFactory
import com.intellij.codeInsight.intention.impl.BaseIntentionAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.{PsiComment, PsiFile}

class InsertHoleTypeAsComment(expr: String) extends BaseIntentionAction {

  override def getText: String = "Insert hole type as comment"

  override def getFamilyName: String = getText

  // TODO: Add a setting for this
  override def isAvailable(project: Project, editor: Editor, file: PsiFile): Boolean = true

  override def invoke(project: Project, editor: Editor, file: PsiFile): Unit = {
    val currentElement = file.findElementAt(editor.getCaretModel.getOffset)
    val elements =
      HaskellElementFactory
        .createFileFromText(project, s" {- $expr -}")
        .getChildren
    currentElement.getParent.addRangeAfter(
      elements.head, elements.last, currentElement
    )
  }
}

object InsertHoleTypeAsComment {

  val REGEX: Pattern = Pattern.compile(
    "• Found hole:[\u0000|\\s]*(.*)[\u0000|\\s]*(Where:|Or perhaps)",
    Pattern.MULTILINE | Pattern.DOTALL
  )

  def create(m: Matcher): InsertHoleTypeAsComment = {
    new InsertHoleTypeAsComment(getExpr(m))
  }

  private def getExpr(m: Matcher): String = {
    m.group(1).replace('\u0000', '\n')
  }
}
