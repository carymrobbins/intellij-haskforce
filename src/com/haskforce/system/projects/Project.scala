package com.haskforce.system.projects

import java.util.regex.{Matcher, Pattern}

import com.haskforce.system.settings.HaskellBuildSettings
import com.haskforce.system.utils.ExecUtil
import com.haskforce.system.utils.ExecUtil.ExecError
import com.intellij.openapi.project.Project
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
    * Returns the active GHCVersion for the project or an error
    */
  def getGHCVersion : Either[ExecUtil.ExecError, GHCVersion]

  def canEqual(other: Any): Boolean = other.isInstanceOf[Project]

  override def equals(other: Any): Boolean = other match {
    case that: Project =>
      (that canEqual this) &&
        getLocation == that.getLocation
    case _ => false
  }

  override def hashCode(): Int = {
    val state = Seq(getLocation)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }
}

sealed trait PackageManager
object PackageManager {
  case object Cabal extends PackageManager
  case object Stack extends PackageManager
}

case class GHCVersion(major: Int, minor: Int, patch: Int)

object GHCVersion {
  private val GHC_VERSION_REGEX: Pattern = Pattern.compile("(?<major>\\d+)\\.(?<minor>\\d+)\\.(?<patch>\\d+)")
  /**
    * Used for parsing the GHC version from `ghc --numeric-version`.
    * for example {@code $ stack ghc -- --numeric-version}<br/>
    * 7.10.3
    * @param input the ghc output
    * @return the version if parsed
    */
  def getGHCVersion(input: String): Option[GHCVersion] = {
    val matcher: Matcher = GHC_VERSION_REGEX.matcher(input)
    if (matcher.matches()) {
      val major: Int = matcher.group("major").toInt
      val minor: Int = matcher.group("minor").toInt
      val patch: Int = matcher.group("patch").toInt
      Some(GHCVersion(major, minor, patch))
    } else {
      None
    }
  }

  /**
    * Returns the active GHCVersion for the
    */
  def getGHCVersion(workingDir: String, path: String) : Either[ExecUtil.ExecError, GHCVersion] = {
    ExecUtil.readCommandLine(workingDir, path, "--numeric-version")
      .right.flatMap(input => {
      GHCVersion.getGHCVersion(input) match {
        case Some(x) => Right(x)
        case None => Left(new ExecError("Unable to parse GHC version input", null))
      }
    })

  }
}
