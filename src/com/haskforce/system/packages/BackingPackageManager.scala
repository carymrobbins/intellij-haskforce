package com.haskforce.system.packages

import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile

/**
  * the Backing package-Manager (e.g. stack or cabal)
  */
trait BackingPackageManager {
  /**
    * returns true if the backing PackageManager is the same (e.g. both stack)
    */
  def sameBacking(other: BackingPackageManager): Boolean

  /**
    * replaces the Main-Package
    * @return the left Error is an Critical Error,
    *         the right is successful replacement with a list of Errors that will be displayed as notifications
    */
  def replaceMain(file : VirtualFile, old: Option[HPackage], project: Project): Either[FileError, List[FileError]]
}
