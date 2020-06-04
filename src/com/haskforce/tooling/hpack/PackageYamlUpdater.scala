package com.haskforce.tooling.hpack

import com.haskforce.highlighting.annotation.external.GhcModi
import com.haskforce.utils.FileUtil
import com.intellij.psi.PsiFile

/**
 * TODO: This provides tools to update a package.yaml _and_ reload
 * external tools. The latter part is a total hack and we should do
 * this in a better way, likely with some sort of framework -
 *    https://www.jetbrains.org/intellij/sdk/docs/tutorials/framework.html
 * There's a branch called stack-manager which has started on this path.
 */
object PackageYamlUpdater {

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
    */
  private def reloadExternalTools(packageYaml: PsiFile): Unit = {
    GhcModi.get(packageYaml).foreach { _.restart() }
  }
}
