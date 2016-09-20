package com.haskforce.system.projects

/**
  * Holds information about the project
  */
trait Project {

  /**
    * Returns the name of the project
    */
  def getName: Option[String]

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
