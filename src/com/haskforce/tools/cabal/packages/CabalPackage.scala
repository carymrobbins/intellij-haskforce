package com.haskforce.tools.cabal.packages

import com.haskforce.system.packages.BuildType.{Benchmark, Executable, Library, TestSuite}
import com.haskforce.system.packages.{BackingPackageManager, BuildInfo, GHCVersion, HPackage, HPackageState, ProjectInformation}
import com.haskforce.system.settings.HaskellBuildSettings
import com.haskforce.system.utils.ExecUtil.ExecError
import com.haskforce.system.utils.{ExecUtil, FileUtil, NonEmptySet, PQ}
import com.haskforce.tools.cabal.lang.psi
import com.haskforce.tools.cabal.lang.psi.{Benchmark, Executable, TestSuite}
import com.intellij.openapi.application.ApplicationManager
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.Project
import com.intellij.openapi.util.Computable
import com.intellij.openapi.vfs.VirtualFile

/**
  * A Cabal Package
  */
class CabalPackage(psiFile: psi.CabalFile, location: VirtualFile) extends HPackage {
  private val LOG = Logger.getInstance(classOf[CabalPackage])

  override def getName: Option[String] = runReadAction(() =>
    for {
      pkgName <- PQ.getChildOfType(psiFile, classOf[psi.PkgName])
      ff <- PQ.getChildOfType(pkgName, classOf[psi.Freeform])
    } yield ff.getText
  )


  override def getLocation: VirtualFile = location

  /**
    * If 'library' stanza exists, returns it; otherwise, implicitly uses root stanza.
    */
  def getLibrary: cabalBuildInfo = runReadAction(() =>
    psiFile.getChildren.collectFirst {
      case c: psi.Library => new cabalBuildInfo(c, Library)
    }.getOrElse(new cabalBuildInfo(psiFile, Library))
  )

  /**
    * Returns the associated BuildInfos
    */
  override def getBuildInfo: NonEmptySet[BuildInfo] = {
    NonEmptySet(getLibrary: BuildInfo).append(psiFile.getChildren.collect {
      case c: Executable => new cabalBuildInfo(c, Executable): BuildInfo
      case c: TestSuite => new cabalBuildInfo(c, TestSuite): BuildInfo
      case c: Benchmark => new cabalBuildInfo(c, Benchmark): BuildInfo
    }.toSet)
  }

  override def getPackageManager: BackingPackageManager = CabalPackageManager

  override def getGHCVersion: Either[ExecUtil.ExecError, GHCVersion] = {
    val project: Project = psiFile.getProject
    val settings: HaskellBuildSettings = HaskellBuildSettings.getInstance(project)
    val path: Either[ExecUtil.ExecError, String] = settings.getGhcPath match {
      case null => Left(new ExecError("No GHC-path configured", null))
      case "" => Left(new ExecError("GHC path is empty", null))
      case x => Right(x)
    }

    path
      .right.flatMap(path => GHCVersion.getGHCVersion(null, path))
  }

  /**
    * returns information about the project (if existing)
    */
  override def getProjectInformation: Option[ProjectInformation] = None

  override def getState: HPackageState = {
    new CabalPackageState(location.getName)
  }

  private def runReadAction[A](f: () => A): A = {
    ApplicationManager.getApplication.runReadAction(new Computable[A] {
      override def compute(): A = f()
    })
  }
}

@SerialVersionUID(23L)
//be careful when editing this file, it gets serialized
class CabalPackageState(cabalFile: String) extends HPackageState with Serializable {
  def getPackageManager = CabalPackageManager.getName
  def getCabalFile = cabalFile
}
