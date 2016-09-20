package com.haskforce.system.packages

import com.intellij.openapi.vfs.VirtualFile

/**
  * Holds information about a haskell package (essentially a stack or cabal file)
  */
trait HPackage {

  /**
    * Returns the name of the package
    */
  def getName: Option[String]

  /**
    * Returns the location of the package
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
