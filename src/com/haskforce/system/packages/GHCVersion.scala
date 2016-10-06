package com.haskforce.system.packages

import java.util.regex.{Matcher, Pattern}

import com.haskforce.system.utils.ExecUtil
import com.haskforce.system.utils.ExecUtil.ExecError

/**
  * represents an specifiv, numerical GHC-version
  */
case class GHCVersion(major: Int, minor: Int, patch: Int)

object GHCVersion {
  private val GHC_VERSION_REGEX: Pattern = Pattern.compile("(?<major>\\d+)\\.(?<minor>\\d+)\\.(?<patch>\\d+)")
  /**
    * Used for parsing the GHC version from `ghc --numeric-version`.
    * for example: <br/>`stack ghc -- --numeric-version`<br/>
    * `7.10.3`
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