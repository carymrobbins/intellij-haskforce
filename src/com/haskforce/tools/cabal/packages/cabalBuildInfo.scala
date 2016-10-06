package com.haskforce.tools.cabal.packages

import com.haskforce.system.packages.{BuildType, BuildInfo => BaseBuildInfo}
import com.haskforce.system.utils.PQ
import com.haskforce.tools.cabal.lang.psi
import com.haskforce.tools.cabal.lang.psi.CabalTypes
import com.haskforce.tools.cabal.lang.psi.impl.SourceDirsImpl
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.util.Computable
import com.intellij.psi.PsiElement

/**
  * A Cabal BuildInfo
  */
class cabalBuildInfo(val el: PsiElement, val bType : BuildType) extends BaseBuildInfo {
  override val typ: BuildType = bType

  /**
    * Returns all listed haskell-extensions.
    */
  override def getExtensions: Set[String] = {
    runReadAction(() =>
      PQ.streamChildren(el, classOf[psi.impl.ExtensionsImpl]).flatMap(
        PQ.getChildOfType(_, classOf[psi.IdentList])
      ).flatMap(
        PQ.getChildNodes(_, CabalTypes.IDENT).map(_.getText)
      ).toSet
    )
  }

  /**
    * Returns the aggregated dependencies' package names.
    */
  override def getDependencies: Set[String] = {
    runReadAction(() =>
      PQ.getChildOfType(el, classOf[psi.BuildDepends]).map(
        _.getPackageNames.toSet
      ).getOrElse(Set.empty)
    )
  }

  /**
    * Returns the aggregated GHC-Options
    */
  override def getGhcOptions: Set[String] = {
    runReadAction(() =>
      PQ.streamChildren(el, classOf[psi.impl.GhcOptionsImpl]).flatMap(_.getValue).toSet
    )
  }

  /**
    * Get hs-source-dirs listed, defaulting to "." if not present.
    */
  override def getSourceDirs: Set[String] = {
    runReadAction(() => {
      val map: Stream[Set[String]] = PQ.streamChildren(el, classOf[SourceDirsImpl]).map(_.getValue.toSet)
      val result: Set[String] = map.foldRight(Set.empty[String])((s, acc) => acc ++ s)
      if (map.isEmpty) Set(".")
      else result
    })
  }

  private def runReadAction[A](f: () => A): A = {
    ApplicationManager.getApplication.runReadAction(new Computable[A] {
      override def compute(): A = f()
    })
  }
}
