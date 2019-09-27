package com.haskforce.features.intentions

import com.haskforce.highlighting.annotation.external.GhcModi
import com.haskforce.utils.FileUtil
import com.intellij.codeInsight.intention.impl.BaseIntentionAction
import com.intellij.lang.annotation.Annotation
import com.intellij.openapi.editor.Editor
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile

class AddPackageDependency(target: AddPackageDependency.Target)
  extends BaseIntentionAction {

  override def getFamilyName: String =
    "Add a package dependency"

  override def getText: String =
    target.getText

  override def isAvailable(project: Project, editor: Editor, file: PsiFile): Boolean =
    // TODO: Add a setting for this
    true

  override def invoke(project: Project, editor: Editor, file: PsiFile): Unit = {
    target match {
      case t: AddPackageDependency.Target.PackageYamlTop => invoke(t)
    }
  }

  // TESTS!
  private def invoke(t: AddPackageDependency.Target.PackageYamlTop): Unit = {
    PackageYamlUpdateUtil.maybeUpdatePackageYamlAndReloadExternalTools(t.packageYaml, text => {
      val lines = text.split('\n').toVector
      lines.zipWithIndex.find {
        case (s, _) => s.trim == "dependencies:"
      }.map { case (_, i) =>
        val (before, after) = lines.splitAt(i + 1)
        // TODO: Try to format this like the others.
        val depLine = "- " + t.dependency
        // TODO: Does this strip newlines?
        // TODO: Does it remove newline at EOF?
        (before ++ (depLine +: after)).mkString("\n")
      }
    })
  }

// TODO: Pure-psi version, hard to work with, unsure how to construct
// YAML nodes since the yaml classes aren't available (closed source?)
//  // TODO: This is horrendous, we need tests!
//  private def invoke(t: AddPackageDependency.Target.PackageYamlTop): Unit = {
//    // TODO: Also, seems like we could traverse the file once to figure
//    SyntaxTraverser.psiTraverser(t.packageYaml).iterator().asScala
//      .filter(_.getText == "dependencies:")
//      .foreach { el =>
//        // Figure out if this is the top-level dependencies.
//        val isTopLevelDeps =
//          Option(el.getParent)
//            .filter(_.toString == "YAML key value")
//            .flatMap(x => Option(x.getParent))
//            .flatMap(x => Option(x.getParent))
//            .exists(_.toString == "YAML document")
//        if (isTopLevelDeps) {
//          val siblings = el.getParent.getChildren
//          val deps = siblings.head.getChildren
//          // TODO: Add pkg dependency to deps
//          return
//        }
//      }
//  }

}

object AddPackageDependency {

  sealed abstract class Target(val getText: String)
  object Target {
    final case class PackageYamlTop(packageYaml: PsiFile, dependency: String)
      extends Target(s"Add $dependency to top-level dependencies in package.yaml")
    // We can add more cases where desired
    // final case class PackageYamlLibrary(...)
    // final case class PackageYamlExecutable(...)
    // final case class CabalLibrary(...)
    // final case class CabalExecutable(...)
    // etc.
  }

  def registerFixes(pkg: String, annotation: Annotation, psiFile: PsiFile): Unit = {
    PackageYamlFinder.psiForFile(psiFile).foreach { packageYaml =>
      annotation.registerFix(new AddPackageDependency(Target.PackageYamlTop(packageYaml, pkg)))
    }
  }
}

// TODO: Put this in a better place
object PackageYamlFinder {

  def psiForFile(psiFile: PsiFile): Option[PsiFile] = {
    if (psiFile.getName == "package.yaml") return Some(psiFile)
    var dir = psiFile.getParent
    while (true) {
      dir.getChildren.foreach {
        case x: PsiFile if x.getName == "package.yaml" => return Some(x)
        case _ => ()
      }
      dir.getParent match {
        case null => return None
        case d => dir = d
      }
    }
    throw new AssertionError()
  }
}

// TODO: Very bad name; also, put this in a better place
object PackageYamlUpdateUtil {

  /**
    * Update the given package.yaml file with the given function.
    * Also, reload external tools if the package.yaml file was changed
    * so they can pick up the changes.
    */
  def maybeUpdatePackageYamlAndReloadExternalTools(
    packageYaml: PsiFile,
    updatePackageYaml: com.intellij.util.Function[String, Option[String]]
  ): Unit = {
    // Update the text of package.yaml; unfortunately, it's not clear how
    // to tell if `updatePackageYaml` returned Some or None outside of
    // this scope, so the successive `reloadExternalTools` can't optimize
    // for it.
    FileUtil.maybeUpdateFileText(
      packageYaml.getProject,
      packageYaml,
      updatePackageYaml,
      () => reloadExternalTools(packageYaml)
    )
  }

  /**
    * Given a PsiFile, find the appropriate package.yaml file and update
    * it with the given function. If the package.yaml is found and an update
    * occurs, also reload any external tools (e.g. ghc-modi).
    */
  def maybeFindAndUpdatePackageYamlAndReloadExternalTools(
    psiFile: PsiFile,
    updatePackageYaml: com.intellij.util.Function[String, Option[String]]
  ): Unit = {
    PackageYamlFinder.psiForFile(psiFile).foreach(
      maybeUpdatePackageYamlAndReloadExternalTools(_, updatePackageYaml)
    )
  }

  /**
    * Attempts to reload any tools that require reloading on a change
    * to the specified package.yaml file.
    * TODO: This is really a big hack and should probably use a framework
    * Note: There's a branch called stack-manager which has started on this path.
    */
  private def reloadExternalTools(packageYaml: PsiFile): Unit = {
    GhcModi.get(packageYaml).foreach { _.restart() }
  }
}
