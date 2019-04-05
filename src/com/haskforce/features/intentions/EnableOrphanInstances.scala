package com.haskforce.features.intentions

import com.haskforce.utils.FileUtil
import com.intellij.codeInsight.intention.impl.BaseIntentionAction
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile

class EnableOrphanInstances extends BaseIntentionAction {

  override def getFamilyName: String =
    "Enable orphan instances via -fno-warn-orphans"

  override def getText: String =
    "Enable -fno-warn-orphans for this file"

  override def isAvailable(project: Project, editor: Editor, file: PsiFile): Boolean =
    // TODO: Add a setting for this
    true

  override def invoke(project: Project, editor: Editor, file: PsiFile): Unit = {
    FileUtil.updateFileText(project, file, text =>
      "{-# OPTIONS_GHC -fno-warn-orphans #-}\n" + text
    )
  }
}
