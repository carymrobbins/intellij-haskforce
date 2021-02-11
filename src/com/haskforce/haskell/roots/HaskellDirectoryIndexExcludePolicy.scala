package com.haskforce.haskell.roots

import com.intellij.openapi.project.Project
import com.intellij.openapi.roots.impl.DirectoryIndexExcludePolicy

import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{Files, Path, Paths}

/**
 * Prevent indexing of directories, namely '.stack-work' and 'dist'.
 * This greatly improves indexing performance.
 */
class HaskellDirectoryIndexExcludePolicy(
  project: Project
) extends DirectoryIndexExcludePolicy {

  override def getExcludeUrlsForProject: Array[String] = {
    val start = Paths.get(project.getBasePath)
    // In tests this may point to a non-existent directory, so avoid a
    // NoSuchFileException we bail out early.
    if (!start.toFile.exists()) return Array.empty
    Files
      .find(start, HaskellDirectoryIndexExcludePolicy.MAX_DEPTH, matcher)
      .map[String](_.toUri.toString)
      .toArray(new Array[String](_))
  }

  private def matcher(path: Path, attrs: BasicFileAttributes): Boolean = {
    attrs.isDirectory &&
      HaskellDirectoryIndexExcludePolicy.EXCLUDED_DIRS.contains(path.getFileName.toString)
  }
}

object HaskellDirectoryIndexExcludePolicy {
  private val MAX_DEPTH = 100
  private val EXCLUDED_DIRS = Set(".stack-work", "dist")
}
