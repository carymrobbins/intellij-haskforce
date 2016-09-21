package com.haskforce.system.projects

import java.util.regex.{Matcher, Pattern}

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

  /**
    * Returns the active GHCVersion for the
    */
  def getGHCVersion : Option[GHCVersion]
}

sealed trait PackageManager
object PackageManager {
  case object Cabal extends PackageManager
  case object Stack extends PackageManager
}

case class GHCVersion(major: Int, minor: Int, patch: Int)

object GHCVersion {
  private val GHC_VERSION_REGEX: Pattern = Pattern.compile("version (?<major>\\d+)\\.(?<minor>\\d+)\\.(?<patch>\\d+)")
  /**
    * Used for parsing the GHC version from `ghc --version`.
    * for example {@code $ ghc-mod --version}<br/>
    * the Glorious Glasgow Haskell Compilation System, version 8.0.1
    * @param input the ghc output
    * @return the version if parsed
    */
  def getGHCVersion(input: String): Option[GHCVersion] = {
    val matcher: Matcher = GHC_VERSION_REGEX.matcher(input)
    if (matcher.find()) {
      val major: Int = matcher.group("major").toInt
      val minor: Int = matcher.group("minor").toInt
      val patch: Int = matcher.group("patch").toInt
      Some(GHCVersion(major, minor, patch))
    } else {
      None
    }
  }
}
