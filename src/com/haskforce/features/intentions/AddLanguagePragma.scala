package com.haskforce.features.intentions

import com.haskforce.psi.HaskellFile
import com.haskforce.utils.FileUtil
import com.intellij.codeInsight.intention.impl.BaseIntentionAction
import com.intellij.lang.annotation.Annotation
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile

class AddLanguagePragma(target: AddLanguagePragma.Target) extends BaseIntentionAction {

  override def getFamilyName: String =
    "Add language pragma"

  override def getText: String = target.getText

  override def isAvailable(project: Project, editor: Editor, file: PsiFile): Boolean =
    // TODO: Add setting
    true

  override def invoke(project: Project, editor: Editor, file: PsiFile): Unit = {
    target match {
      case t: AddLanguagePragma.Target.HaskellFilePragma => invoke(t)
      case t: AddLanguagePragma.Target.PackageYamlTop => invoke(t)
    }
  }

  private def invoke(t: AddLanguagePragma.Target.HaskellFilePragma): Unit = {
    // TODO: Insert it sorted, as best as we can.
    FileUtil.updateFileText(t.haskellFile.getProject, t.haskellFile, text => {
      s"{-# LANGUAGE ${t.extension} #-}\n" + text
    })
  }

  private def invoke(t: AddLanguagePragma.Target.PackageYamlTop): Unit = {
    // TODO: Insert it sorted, as best as we can.
    PackageYamlUpdateUtil.maybeUpdatePackageYamlAndReloadExternalTools(t.packageYaml, text => {
      // TODO: Fix this copy pasta; adapted from AddPackageDependency.
      val lines = text.split('\n').toVector
      lines.zipWithIndex.find {
        case (s, _) => s.trim == "default-extensions:"
      }.map { case (_, i) =>
        val (before, after) = lines.splitAt(i + 1)
        // TODO: Try to format this like the others.
        val depLine = "- " + t.extension
        // TODO: Does this strip newlines?
        // TODO: Does it remove newline at EOF?
        (before ++ (depLine +: after)).mkString("\n")
      }
    })
  }
}

object AddLanguagePragma {

  sealed abstract class Target(val getText: String)
  object Target {
    final case class HaskellFilePragma(haskellFile: HaskellFile, extension: String)
      extends Target(s"Add LANGUAGE $extension pragma to this file")
    final case class PackageYamlTop(packageYaml: PsiFile, extension: String)
      extends Target(s"Add $extension to top-level default-extensions in package.yaml")
  }

  def registerFixes(extension: String, annotation: Annotation, psiFile: PsiFile): Unit = {
    psiFile match {
      case haskellFile: HaskellFile =>
        annotation.registerFix(new AddLanguagePragma(Target.HaskellFilePragma(haskellFile, extension)))
      case _ =>
        // noop
    }
    PackageYamlFinder.psiForFile(psiFile).foreach { packageYaml =>
      annotation.registerFix(new AddLanguagePragma(Target.PackageYamlTop(packageYaml, extension)))
    }
  }
}
