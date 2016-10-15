package com.haskforce.system.packages

/**
  * Holds information about a particular build
  */
trait BuildInfo {
  /**
    * the Type of the Build
    */
  val typ: BuildType

  /**
    * Returns all listed haskell-extensions.
    */
  def getExtensions: Set[String]

  /**
    * Returns the aggregated dependencies' package names.
    */
  def getDependencies: Set[String]

  /**
    * Returns the aggregated GHC-Options
    */
  def getGhcOptions: Set[String]

  /**
    * Get hs-source-dirs listed, defaulting to the directory of the stack/cabal file if not present
    */
  def getSourceDirs: Set[String]
}

/**
  * The type of the Build
  */
sealed trait BuildType
object BuildType {
  case object Library extends BuildType
  case object Executable extends BuildType
  case object TestSuite extends BuildType
  case object Benchmark extends BuildType
}
