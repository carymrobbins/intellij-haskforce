package com.haskforce.tools.stack.projects

import com.haskforce.system.projects.{GHCVersion, PackageManager}
import com.haskforce.system.settings.HaskellBuildSettings
import com.haskforce.system.utils.ExecUtil
import com.haskforce.system.utils.ExecUtil.ExecError
import com.haskforce.tools.cabal.lang.psi
import com.haskforce.tools.cabal.projects.CabalProject
import com.intellij.openapi.vfs.VirtualFile

/**
  * A Stack Project
  */
class StackProject(psiFile: psi.CabalFile, stackFile: VirtualFile) extends CabalProject(psiFile) {
  /**
    * Returns the corresponding PackageManager
    */
  override def getPackageManager: PackageManager = PackageManager.Stack

  /**
    * Returns the active GHCVersion for the
    */
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
}
