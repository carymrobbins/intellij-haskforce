package com.haskforce.tools.cabal.packages

import java.io.File

import com.haskforce.system.packages.{HPackageManager, HPackage}
import com.haskforce.tools.cabal.lang.psi.CabalFile
import com.intellij.openapi.diagnostic.Logger
import com.intellij.openapi.project.{Project => IProject, ProjectManager => IProjectManager}
import com.intellij.openapi.vfs.{LocalFileSystem, VirtualFile}
import com.intellij.psi.PsiManager

/**
  * utility class used to create/delete Cabal projects
  */
object CabalPackagesManager {

  private val LOG = Logger.getInstance(CabalPackagesManager.getClass)

  /**
    * registers the new Cabal-Package
    * @param file the file pointing to the cabal-file
    * @return either the RegisterError or the Package
    */
  def registerNewPackage(file: File) : Either[RegisterError, HPackage] = {
    val virtualFile: VirtualFile = LocalFileSystem.getInstance().findFileByIoFile(file)
    if (virtualFile == null) {
      Left(FileError(file.getCanonicalPath, file.getName, s"unable to obtain VirtualFile for file ${file.getAbsolutePath}"))
    } else {
      registerNewPackage(virtualFile)
    }
  }

  /**
    * registers the new Cabal-Package
    * @param file the VirtualFile pointing to the cabal-file
    * @return either the RegisterError or the Package
    */
  def registerNewPackage(file : VirtualFile) : Either[RegisterError, HPackage] = {
    val defaultProject: IProject = IProjectManager.getInstance().getDefaultProject
    PsiManager.getInstance(defaultProject).findFile(file) match {
      case psiFile: CabalFile => {
        val cabalProject: CabalPackage = new CabalPackage(psiFile)
        val added: Boolean = HPackageManager.addPackage(cabalProject)
        if (added) {
          Right(cabalProject)
        } else {
          Left(AlreadyRegistered(cabalProject))
        }
      }
      case other =>
        Left(FileError(file.getCanonicalPath, file.getNameWithoutExtension, s"Expected CabalFile, got: ${other.getClass}"))
    }
  }
}

sealed trait RegisterError
case class FileError(location: String, fileName : String, errorMsg: String) extends RegisterError
case class AlreadyRegistered(project: HPackage) extends RegisterError

