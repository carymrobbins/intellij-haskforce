package com.haskforce.tooling.ghcPkg

import com.intellij.openapi.project.Project

class GhcPkgDumpProjectCacheService {

  def get: Option[CachedPkgs] = cachedPkgs

  def put(cachedPkgs: CachedPkgs): Unit = {
    this.cachedPkgs = Some(cachedPkgs)
  }

  private var cachedPkgs: Option[CachedPkgs] = None
}

object GhcPkgDumpProjectCacheService {
  def getInstance(project: Project): GhcPkgDumpProjectCacheService = {
    project.getService(classOf[GhcPkgDumpProjectCacheService])
  }
}
