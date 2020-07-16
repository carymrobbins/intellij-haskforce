package com.haskforce.tooling.ghcPkg

import java.util.concurrent.ConcurrentHashMap

import com.intellij.openapi.components.ServiceManager
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.vfs.VirtualFile

import scala.collection.JavaConverters._

class GhcPkgDumpProjectCacheService(project: Project) {

  def getPkgs: Option[CachedPkgs] = cachedPkgs

  def putPkgs(cachedPkgs: CachedPkgs): Unit = {
    this.cachedPkgs = Some(cachedPkgs)
  }

  val sourcePathDependencyIndex = new GhcPkgDumpProjectCacheService.SourcePathDependencyIndex(project)

  private var cachedPkgs: Option[CachedPkgs] = None
}

object GhcPkgDumpProjectCacheService {

  def getInstance(project: Project): GhcPkgDumpProjectCacheService = {
    project.getService(classOf[GhcPkgDumpProjectCacheService])
  }

  final class SourcePathDependencyIndex(project: Project) {

    private val internal = new ConcurrentHashMap[String, ConcurrentHashMap[Pkg, Unit]]()

    def clear(): Unit = {
      internal.clear()
    }

    def getDependenciesForFile(vFile: VirtualFile): Option[Set[Pkg]] = {
      getDependenciesForSourcePath(vFile.getCanonicalPath)
        .orElse(getDependenciesForSourceFolder(vFile))
    }

    private def getDependenciesForSourceFolder(vFile: VirtualFile): Option[Set[Pkg]] = {
      for {
        srcFolder <- Option(
          ProjectRootManager.getInstance(project)
            .getFileIndex.getSourceFolder(vFile)
        )
        srcFolderFile <- Option(srcFolder.getFile)
        srcFolderPath <- Option(srcFolderFile.getCanonicalPath)
        pkgs <- getDependenciesForSourcePath(srcFolderPath)
      } yield pkgs
    }

    def addDependencyForSourcePath(srcPath: String, pkgsToAdd: Iterable[Pkg]): Unit = {
      internal.compute(srcPath, (_, pkgsOrNull) => {
        val pkgs = if (pkgsOrNull == null) new ConcurrentHashMap[Pkg, Unit]() else pkgsOrNull
        pkgsToAdd.foreach(pkgs.put(_, ()))
        pkgs
      })
      ()
    }

    def getDependenciesForSourcePath(srcPath: String): Option[Set[Pkg]] = {
      val m = internal.get(srcPath)
      if (m == null) return None
      Some(m.keys().asScala.toSet)
    }
  }
}
