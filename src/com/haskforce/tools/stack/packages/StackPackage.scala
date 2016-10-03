package com.haskforce.tools.stack.packages

import java.util

import com.haskforce.system.packages._
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
class StackPackage(
                    psiFile: psi.CabalFile,
                    location: VirtualFile,
                    stackFile: VirtualFile,
                    stackPathFromProject: String,
                    allPackages: mutable.ListBuffer[StackPackage],
                    buildDir: List[VirtualFile]
                  ) extends CabalPackage(psiFile, location) {

  lazy val stackInfo: StackProjectInformation = new StackProjectInformation(allPackages.toList, buildDir)

  override def getPackageManager: BackingPackageManager = StackPackageManager

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

  override def getProjectInformation: Option[ProjectInformation] = Some(stackInfo)

  override def getState: HPackageState = {
    new StackPackageState(stackPathFromProject, location.getName)
  }
}

class StackProjectInformation(packages: List[StackPackage], buildDir: List[VirtualFile]) extends ProjectInformation {

  override def getBuildDirectories: List[VirtualFile] = buildDir

  override def getRelatedPackages: List[HPackage] = packages
}

//be careful when editing this file, it gets serialized
class StackPackageState(stackPath: String, cabalName: String) extends HPackageState with Serializable {
  override def getPackageManager: String = StackPackageManager.getName
  def getStackPath = stackPath
  def getCabalName = cabalName
}
