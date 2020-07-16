package com.haskforce.codeInsight.visibleModules

import com.haskforce.tooling.ghcPkg.GhcPkgDumpProjectCacheService
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiFile

class GhcPkgDumpVisibleModulesProvider private(
  project: Project,
  vFile: VirtualFile
) extends VisibleModulesProvider {

  override def getVisibleModules: Array[String] = {
    GhcPkgDumpProjectCacheService.getInstance(project)
      .sourcePathDependencyIndex
      .getDependenciesForVirtualFile(vFile)
      .map { pkgs =>
        pkgs.iterator.flatMap(_.exposedModules).toArray
      }.getOrElse(Array.empty)
  }
}

object GhcPkgDumpVisibleModulesProvider {

  def create(psiFile: PsiFile): Option[GhcPkgDumpVisibleModulesProvider] = {
    for {
      vFile <- Option(psiFile.getOriginalFile.getVirtualFile)
    } yield new GhcPkgDumpVisibleModulesProvider(
      psiFile.getProject,
      vFile,
    )
  }
}
