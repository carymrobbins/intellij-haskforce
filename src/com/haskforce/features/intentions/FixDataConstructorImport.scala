package com.haskforce.features.intentions

import com.haskforce.highlighting.annotation.external.GhcMod.Problem
import com.haskforce.psi.HaskellImportt
import com.haskforce.psi.impl.HaskellElementFactory
import com.intellij.codeInsight.intention.impl.BaseIntentionAction
import com.intellij.openapi.editor.{Editor, LogicalPosition}
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile
import com.intellij.psi.util.PsiTreeUtil

class FixDataConstructorImport(
  constructorName: String,
  typeName: String,
  problem: Problem
) extends BaseIntentionAction {

  override def getFamilyName: String =
    "Fix missing type data constructor import"

  override def isAvailable(project: Project, editor: Editor, file: PsiFile): Boolean =
    // TODO: Add a setting for this
    true

  override def invoke(project: Project, editor: Editor, file: PsiFile): Unit = {
    val el = file.findElementAt(
      editor.logicalPositionToOffset(new LogicalPosition(
        problem.startColumn + 1,
        problem.startLine + 1
      ))
    )

    val importt = HaskellElementFactory.createFileFromText(
      project,
      s"import X $typeName($constructorName)"
    ).findChildByClass(classOf[HaskellImportt])

    PsiTreeUtil
      .findFirstParent(el, _.isInstanceOf[HaskellImportt])
      .replace(importt)
  }
}
