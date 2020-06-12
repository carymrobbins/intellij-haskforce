package com.haskforce.features.intentions

import com.haskforce.tooling.hpack.PackageYamlFinder
import com.intellij.codeInsight.intention.impl.BaseIntentionAction
import com.intellij.lang.annotation.Annotation
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.fileEditor.FileEditorManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiFile

class EditPackageYaml(
  description: String,
  packageYaml: VirtualFile
) extends BaseIntentionAction {

  override def getFamilyName: String =
    "Edit package yaml"

  override def getText: String = description

  override def isAvailable(project: Project, editor: Editor, file: PsiFile): Boolean =
    // TODO: Add setting
    true

  override def invoke(project: Project, editor: Editor, file: PsiFile): Unit = {
    FileEditorManager.getInstance(project).openFile(packageYaml, true, true)
    ()
  }
}

object EditPackageYaml {

  def registerFixes(description: String, annotation: Annotation, file: PsiFile): Unit = {
    PackageYamlFinder.psiForFile(file)
      .flatMap(x => Option(x.getVirtualFile))
      .foreach { packageYaml =>
        annotation.registerFix(new EditPackageYaml(description, packageYaml))
      }
  }
}
