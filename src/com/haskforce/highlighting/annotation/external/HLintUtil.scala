package com.haskforce.highlighting.annotation.external

import com.haskforce.ui.tools.HaskellToolsConsole
import com.haskforce.utils.ExecUtil.ExecError
import scala.util.Try
import scalaz.std.either._
import scalaz.std.list._
import scalaz.syntax.bifunctor._
import scalaz.syntax.traverse._

object HLintUtil {

  def parseVersion(version: String):Either[ExecError, VersionTriple] = {
    version.trim.split('.').take(3).toList
      .traverseU(x => Try(x.toInt).toEither)
      .flatMap {
        case Nil => Left(new IllegalArgumentException("No version digits found"))
        case x :: yz => Right((x, yz.lift(0), yz.lift(1)))
      }
      .bimap(
        new ExecError(s"Could not parse version from hlint: '$version'", _),
        v => VersionTriple(v._1, v._2.getOrElse(0), v._3.getOrElse(0))
      )
  }

  def runHLintNumericVersion(
    toolConsole: HaskellToolsConsole,
    workDir: String,
    hlintPath: String
  ): Either[ExecError, VersionTriple] = {
    HLint.runHlint(toolConsole, workDir, hlintPath, "--numeric-version")
      .flatMap(parseVersion)
  }
}
