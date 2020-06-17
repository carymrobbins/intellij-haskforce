package com.haskforce.haskell.roots

import java.nio.file.{Files, Paths}

import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.impl.DirectoryIndexExcludePolicy

/**
 * Prevent indexing of directories, namely '.stack-work' and 'dist'.
 * This greatly improves indexing performance.
 */
class HaskellDirectoryIndexExcludePolicy(
  project: Project
) extends DirectoryIndexExcludePolicy {

  override def getExcludeUrlsForProject: Array[String] = {
    Files.find(
      Paths.get(project.getBasePath),
      HaskellDirectoryIndexExcludePolicy.MAX_DEPTH,
      (path, attrs) => (
        attrs.isDirectory
          && HaskellDirectoryIndexExcludePolicy.EXCLUDED_DIRS.contains(path.getFileName.toString)
      )
    ).map[String](_.toUri.toString).toArray(new Array[String](_))
  }
}

object HaskellDirectoryIndexExcludePolicy {
  private val MAX_DEPTH = 100
  private val EXCLUDED_DIRS = Set(".stack-work", "dist")
}
