package com.haskforce.codeInsight.visibleModules

import com.haskforce.settings.HaskellBuildSettings
import com.haskforce.settings.experimental.HaskForceExperimentalSettingsConfigurable
import com.haskforce.tooling.ghcPkg.{CachedPkgs, GhcPkgDumpExecutor, GhcPkgDumpParser, GhcPkgDumpProjectCacheService}
import com.haskforce.tooling.hpack.{PackageYamlFinder, PackageYamlQuery}
import com.intellij.ide.util.PropertiesComponent
import com.intellij.openapi.project.Project
import com.intellij.psi.PsiFile

class GhcPkgDumpVisibleModulesProvider private(
  psiFile: PsiFile,
  project: Project,
  projectDir: String,
  stackPath: String
) extends VisibleModulesProvider {

  override def getVisibleModules: Array[String] = {
    getPackageDeps() match {
      case None => Array.empty
      case Some(pkgDeps) =>
        val pkgs = getCachedPkgs()
        getVisibleModules(pkgDeps, pkgs)
    }
  }

  // TODO: A hack until we get proper stack framework support.
  private def getPackageDeps(): Option[List[String]] = {
    PackageYamlFinder.psiForFile(psiFile).flatMap(packageYaml =>
      PackageYamlQuery.getTopLevelDeps(packageYaml)
    )
  }

  private def getVisibleModules(
    pkgDeps: List[String],
    pkgs: CachedPkgs
  ): Array[String] = {
    pkgDeps.iterator
      .flatMap(k => pkgs.named(k).iterator)
      // TODO: Hack - we just grab the first version, need to check it,
      // but this will be easier once we have stack framework support.
      .flatMap(_.head.iterator)
      // TODO: It would be nice to report the package name somehow, too.
      // That would require changing the interface, though.
      .flatMap(_.exposedModules.iterator)
      .toArray[String]
  }

  private def getCachedPkgs(): CachedPkgs = {
    val cacheService = GhcPkgDumpProjectCacheService.getInstance(project)
    cacheService.get.getOrElse {
      val pkgs = runGhcPkgDump()
      cacheService.put(pkgs)
      pkgs
    }
  }

  private def runGhcPkgDump(): CachedPkgs = {
    val is = GhcPkgDumpExecutor.runWithStack(stackPath, projectDir)
    CachedPkgs.fromIterator(GhcPkgDumpParser.parse(is))
  }
}

object GhcPkgDumpVisibleModulesProvider {

  def create(psiFile: PsiFile): Option[GhcPkgDumpVisibleModulesProvider] = {
    val props = PropertiesComponent.getInstance(psiFile.getProject)
    val experimentalSettings =
      HaskForceExperimentalSettingsConfigurable.State.load(props)
    if (!experimentalSettings.ghcPkgEnabled) {
      return None
    }
    val settings = HaskellBuildSettings.getInstance(psiFile.getProject)
    val project = psiFile.getProject
    for {
      projectDir <- Option(project.getBasePath)
      stackPath <- Option(settings.getStackPath)
    } yield new GhcPkgDumpVisibleModulesProvider(
      psiFile,
      project,
      projectDir,
      stackPath
    )
  }
}
