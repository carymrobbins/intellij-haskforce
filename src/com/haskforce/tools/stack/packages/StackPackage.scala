package com.haskforce.tools.stack.packages

import com.haskforce.system.packages.{GHCVersion, PackageManager}
import com.haskforce.system.settings.HaskellBuildSettings
import com.haskforce.system.utils.ExecUtil
import com.haskforce.system.utils.ExecUtil.ExecError
import com.haskforce.tools.cabal.lang.psi
import com.haskforce.tools.cabal.packages.CabalPackage
import com.intellij.openapi.vfs.VirtualFile

import scala.collection.mutable

/**
  * A Stack Project
  */
class StackPackage(psiFile: psi.CabalFile, stackFile: VirtualFile, allPackages: mutable.MutableList[StackPackage]) extends CabalPackage(psiFile) {
  lazy val immutableAllPackagesList = allPackages.toList

  override def getPackageManager: PackageManager = PackageManager.Stack

  override def getGHCVersion: Either[ExecError, GHCVersion] = {
    val buildSettings: HaskellBuildSettings = HaskellBuildSettings.getInstance(psiFile.getProject)
    ExecUtil.readCommandLine(stackFile.getParent.getPath, buildSettings.getStackPath, "ghc", "--", "--numeric-version")
      .right.flatMap(input => {
      GHCVersion.getGHCVersion(input) match {
        case Some(x) => Right(x)
        case None => Left(new ExecError("Unable to parse GHC version input", null))
      }
    })
  }

  /**
    * returns all the packages of the associated Stack project
    * @return a list of packages
    */
  def getAllPackages: List[StackPackage] = immutableAllPackagesList
}
