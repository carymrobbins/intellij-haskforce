package com.haskforce.system.packages

import com.intellij.openapi.project.Project
import com.intellij.openapi.vfs.VirtualFile

/**
  * the Backing package-Manager (e.g. stack or cabal)
  */
trait BackingPackageManager {

  /**
    * a unique name of the Package-Manager
    */
  def getName: String

  /**
    * returns the hPackage from the persisted state
    */
  def getPackageFromState(hPackageState: HPackageState, moduleRoots: List[VirtualFile], project: Project): Option[HPackage]

  /**
    * returns true if the backing PackageManager is the same (e.g. both stack)
    */
  def sameBacking(other: BackingPackageManager): Boolean

  /**
    * returns the Packages
    * @param file the cabal/stack file.
    * @return the left Error is an Critical Error,
    *         the right is a list of parsed Packages, where the left packages that are not found etc
    */
  def getPackages(file : VirtualFile, project: Project): Either[FileError, List[Either[FileError, HPackage]]]
}
