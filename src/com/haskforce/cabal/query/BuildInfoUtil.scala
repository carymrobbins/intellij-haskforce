package com.haskforce.cabal.query

import prelude._

import java.util

import com.haskforce.cabal.completion.CabalFileFinder
import com.haskforce.psi.HaskellFile
import com.haskforce.utils.IJReadAction

/** Utility for getting Cabal BuildInfo metadata, particularly from Java. */
object BuildInfoUtil {

  def getBuildInfo(haskellFile: HaskellFile): IJReadAction[Option[BuildInfo]] = {
    for {
      cabalFile <- OptionT(CabalFileFinder.psiForFile(haskellFile))
      q = new CabalQuery(SPsiFile(cabalFile))
      buildInfo <- OptionT(q.findBuildInfoForPsiFile(SPsiFile(haskellFile)))
    } yield buildInfo
  }.run

  def getExtensionOpts(buildInfo: BuildInfo): IJReadAction[util.List[String]] = {
    buildInfo.getExtensions.map { xs =>
      val result = new util.ArrayList[String](xs.size)
      xs.foreach { x => result.add("-X" + x) }
      result
    }
  }

  def getExtensionOpts(o: Option[BuildInfo]): IJReadAction[util.List[String]] = o match {
    case None => IJReadAction(util.Collections.emptyList())
    case Some(bi) => getExtensionOpts(bi)
  }

  def getExtensionOpts(haskellFile: HaskellFile): IJReadAction[util.List[String]] = {
    for {
      buildInfo <- getBuildInfo(haskellFile)
      opts <- getExtensionOpts(buildInfo)
    } yield opts
  }

  def getGhcOpts(buildInfo: BuildInfo): IJReadAction[util.List[String]] = {
    buildInfo.getGhcOptions.map { xs =>
      val result = new util.ArrayList[String](xs.size)
      xs.foreach { result.add }
      result
    }
  }

  def getGhcOpts(o: Option[BuildInfo]): IJReadAction[util.List[String]] = o match {
    case None => IJReadAction(util.Collections.emptyList())
    case Some(bi) => getGhcOpts(bi)
  }
}
