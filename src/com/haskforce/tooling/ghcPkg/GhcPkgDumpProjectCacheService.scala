package com.haskforce.tooling.ghcPkg

import java.util
import java.util.concurrent.ConcurrentHashMap

import com.intellij.openapi.components._
import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.ProjectRootManager
import com.intellij.openapi.vfs.VirtualFile
import com.intellij.psi.PsiFile

import scala.beans.BeanProperty

@State(
  name = "GhcPkgDumpProjectCacheService",
  storages = Array(new Storage("ghc-pkg-dump.xml"))
)
class GhcPkgDumpProjectCacheService(project: Project)
  extends PersistentStateComponent[GhcPkgDumpProjectCacheService.State] {

  import GhcPkgDumpProjectCacheService._

  private var state = new State

  override def getState: State = state

  override def loadState(s: State): Unit = {
    this.state = s
  }

  def getPkgs: Option[CachedPkgs] = state.cachedPkgs

  def putPkgs(cachedPkgs: CachedPkgs): Unit = {
    state.cachedPkgs = Some(cachedPkgs)
  }

  def clearSourcePathDependencyIndex(): Unit = {
    state.sourcePathDependencyIndex.clear()
  }

  def getDependenciesForPsiFile(psiFile: PsiFile): Option[util.Set[Pkg]] = {
    Option(psiFile.getOriginalFile.getVirtualFile).flatMap(getDependenciesForVirtualFile)
  }

  def getDependenciesForVirtualFile(vFile: VirtualFile): Option[util.Set[Pkg]] = {
    getDependenciesForSourcePath(vFile.getCanonicalPath)
      .orElse(getDependenciesForSourceFolder(vFile))
  }

  private def getDependenciesForSourceFolder(vFile: VirtualFile): Option[util.Set[Pkg]] = {
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
    val pkgs = state.sourcePathDependencyIndex.computeIfAbsent(srcPath, _ => new util.HashSet())
    pkgsToAdd.foreach(pkgs.add)
  }

  def getDependenciesForSourcePath(srcPath: String): Option[util.Set[Pkg]] = {
    Option(state.sourcePathDependencyIndex.get(srcPath))
  }
}

object GhcPkgDumpProjectCacheService {

  def getInstance(project: Project): GhcPkgDumpProjectCacheService = {
    project.getService(classOf[GhcPkgDumpProjectCacheService])
  }

  class State(
    @BeanProperty var cachedPkgs: Option[CachedPkgs],
    @BeanProperty var sourcePathDependencyIndex: ConcurrentHashMap[String, util.HashSet[Pkg]]
  ) {
    def this() = this(None, new ConcurrentHashMap())
  }
}
