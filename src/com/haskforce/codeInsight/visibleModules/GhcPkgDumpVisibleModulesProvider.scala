package com.haskforce.codeInsight.visibleModules

import com.haskforce.settings.experimental.HaskForceExperimentalConfigurable
import com.haskforce.tooling.ghcPkg.GhcPkgDumpProjectCacheService
import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiFile

import scala.collection.JavaConverters._

class GhcPkgDumpVisibleModulesProvider private(
  project: Project,
  vFile: VirtualFile
) extends VisibleModulesProvider {

  override def getVisibleModules: Array[String] = {
    GhcPkgDumpProjectCacheService.getInstance(project)
      .getDependenciesForVirtualFile(vFile)
      .map { pkgs =>
        pkgs.iterator.asScala
          .flatMap(_.exposedModules.iterator().asScala)
          .toArray
      }.getOrElse(Array.empty)
  }
}

object GhcPkgDumpVisibleModulesProvider {

  def create(psiFile: PsiFile): Option[GhcPkgDumpVisibleModulesProvider] = {
    val project = psiFile.getProject
    if (!HaskForceExperimentalConfigurable.isGhcPkgEnabled(project)) return None
    for {
      vFile <- Option(psiFile.getOriginalFile.getVirtualFile)
    } yield new GhcPkgDumpVisibleModulesProvider(
      project,
      vFile,
    )
  }
}
