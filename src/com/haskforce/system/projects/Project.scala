package com.haskforce.system.projects

import com.intellij.openapi.vfs.VirtualFile

/**
  * Holds information about a haskell (not intellij) project (essentially a stack or cabal file)
  */
trait Project {

  /**
    * Returns the name of the project
    */
  def getName: Option[String]

  /**
    * Returns the location of the project
    */
  def getLocation: VirtualFile

  /**
    * Returns the associated BuildInfos
    */
  def getBuildInfo: List[BuildInfo]

  /**
    * Returns the corresponding PackageManager
    */
  def getPackageManager: PackageManager
}

sealed trait PackageManager
object PackageManager {
  case object Cabal extends PackageManager
  case object Stack extends PackageManager
}
